import { EventEmitter, Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable, NotImplementedException } from '@nestjs/common';
import { ClientProxy, RmqRecordBuilder } from '@nestjs/microservices';
import {
   CRUDClient,
   EntityResult,
   HttpRequest,
   OnServiceResponse,
   PaginationQueryDto,
   PaginationResult,
   ServiceOptions,
   UserRefEntity,
   eventConstant,
} from '@shared-library';
import { lastValueFrom, timeout } from 'rxjs';
import { REQUEST } from '@nestjs/core';
import { ServiceName, appConfig, clientProxy, serviceConfig } from '@metadata';
import { Language } from '@shared-library/i18n';

@Injectable()
export class BaseClientProxy {
   @Inject(REQUEST) private req: HttpRequest;

   @Inject(Language) private language: Language;

   @Inject(EventEmitter) private eventEmitter: EventEmitter;

   private clientProxy: ClientProxy;

   createClient(serviceName: ServiceName): BaseClientProxy {
      const instance = new BaseClientProxy();
      instance.req = this.req;
      instance.language = this.language;
      instance.eventEmitter = this.eventEmitter;
      instance.clientProxy = clientProxy(serviceName);

      return instance;
   }

   async send<TResult, TInput>(messagePattern: string, data?: TInput, options?: ServiceOptions): Promise<TResult> {
      if (!this.clientProxy) {
         throw new NotImplementedException('The client proxy is not initialized');
      }

      const { user } = this.req;
      const meta = Registry.from()
         .extends({ query: this.req.query })
         .extends(options?.meta ?? {})
         .extends({
            method: this.req.method,
            user: user ? user.toUserRefEntity() : user,
            query: { lang: this.language.code },
         });

      const eventPayload: OnServiceResponse = {
         messagePattern,
         requestData: { data, meta: meta.valueOf() },
         httpRequest: this.req,
         success: true,
      };

      try {
         const record = new RmqRecordBuilder(data).setOptions({ headers: { 'x-meta': meta.toString() } }).build();
         const response = await lastValueFrom(
            this.clientProxy
               .send(messagePattern, record)
               .pipe(timeout(options?.timeOut ?? appConfig.get('apiGateway.requestTimeout'))),
         );

         if (options?.noEmitEvent !== true) {
            // Emit the response success event
            eventPayload.responseData = Util.clone(response);
            this.emitResponseEvent(eventPayload);
         }

         // Check to remove noneeded metadata
         if (Is.object(response?.meta)) {
            const allowedMeta: string[] = ['totalCount', 'page', 'limit'];

            for (const key in response.meta) {
               if (!allowedMeta.includes(key)) {
                  delete response.meta[key];
               }
            }
         }

         return response;
      } catch (e: any) {
         if (options?.noEmitEvent !== true) {
            // Emit the response failure
            eventPayload.success = false;
            eventPayload.responseData = e;
            this.emitResponseEvent(eventPayload);
         }

         throw e;
      }
   }

   private emitResponseEvent(eventPayload: OnServiceResponse) {
      process.nextTick(() =>
         this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload).catch(console.debug),
      );
   }

   createCRUD(patternCRUD: string, options?: ServiceOptions): CRUDClient {
      return {
         read: <TEntity>(id: string, optionsOverride?: ServiceOptions): Promise<EntityResult<TEntity>> =>
            this.send(patternCRUD, id, optionsOverride ?? options),

         paginate: <TEntity>(
            query?: PaginationQueryDto,
            optionsOverride?: ServiceOptions,
         ): Promise<PaginationResult<TEntity>> => this.send(patternCRUD, query, optionsOverride ?? options),

         create: <TEntity, TData>(data: TData, optionsOverride?: ServiceOptions): Promise<EntityResult<TEntity>> =>
            this.send(patternCRUD, data, optionsOverride ?? options),

         update: <TEntity, TData>(
            id: string,
            data: TData,
            optionsOverride?: ServiceOptions,
         ): Promise<EntityResult<TEntity>> => this.send(patternCRUD, { id, data }, optionsOverride ?? options),

         delete: <TEntity>(id: string, optionsOverride?: ServiceOptions): Promise<EntityResult<TEntity>> =>
            this.send(patternCRUD, id, optionsOverride ?? options),
      };
   }

   async validateUserRef<TResult>(
      userId?: string | string[],
      cb?: (...userRefs: UserRefEntity[]) => TResult | Promise<TResult>,
   ): Promise<TResult> {
      let usersRef: UserRefEntity[] = [];

      if (userId) {
         const { name, patterns } = serviceConfig.get('user');
         const userCRUD = this.createClient(name).createCRUD(patterns.userCRUD, { noEmitEvent: true });
         usersRef = await Promise.all(
            (Is.array(userId) ? userId : [userId]).map((uid) =>
               userCRUD.read(uid).then((user) => new UserRefEntity(user)),
            ),
         );
      }

      return Is.callable(cb) ? Util.callAsync(this, cb, ...usersRef) : <TResult>usersRef;
   }
}
