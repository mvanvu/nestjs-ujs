import { EventEmitter, Is, Registry, Util } from '@mvanvu/ujs';
import { Inject, Injectable, NotImplementedException } from '@nestjs/common';
import { ClientProxy, RmqRecordBuilder } from '@nestjs/microservices';
import {
   CRUDClient,
   HttpRequest,
   MessageData,
   OnServiceResponse,
   PaginationQueryDto,
   ServiceOptions,
   eventConstant,
} from '@lib/common';
import { lastValueFrom, timeout } from 'rxjs';
import { REQUEST } from '@nestjs/core';
import { ServiceName, appConfig, clientProxy, serviceConfig } from '@metadata';
import { UserRefEntity } from '@lib/service';

@Injectable()
export class BaseClientProxy {
   @Inject(REQUEST) private req: HttpRequest;

   @Inject(EventEmitter) private eventEmitter: EventEmitter;

   private clientProxy: ClientProxy;

   createClient(serviceName: ServiceName): BaseClientProxy {
      const instance = new BaseClientProxy();
      instance.req = this.req;
      instance.eventEmitter = this.eventEmitter;
      instance.clientProxy = clientProxy(serviceName);

      return instance;
   }

   async send<TResult, TInput>(
      messagePattern: string,
      dataDelivery?: MessageData<TInput>,
      options?: ServiceOptions,
   ): Promise<TResult> {
      if (!this.clientProxy) {
         throw new NotImplementedException('The client proxy is not initialized');
      }

      const eventPayload: OnServiceResponse = {
         messagePattern,
         requestData: dataDelivery?.data,
         httpRequest: this.req,
         success: true,
      };

      try {
         const regReq = this.req.registry.clone();
         const user = regReq.get('user');

         if (user) {
            // Just send some important user data
            regReq.set('user', { id: user.id, username: user.name, email: user.email, group: user.group });
         }

         const record = new RmqRecordBuilder<any>(dataDelivery?.data || {})
            .setOptions({
               headers: {
                  'x-meta': Registry.from({})
                     .extends(dataDelivery?.meta || {})
                     .extends({ headers: regReq.valueOf() })
                     .toString(),
               },
            })
            .build();

         const response = await lastValueFrom(
            this.clientProxy.send(messagePattern, record).pipe(timeout(options?.timeOut ?? 5000)),
         );

         if (options?.noEmitEvent !== true) {
            // Emit the response success event
            eventPayload.responseData = response;
            await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);
         }

         // Check to remove unneeded metadata
         if (Is.object(response) && Is.object(response.meta)) {
            const allowedMeta: string[] = ['totalCount', 'page', 'limit'];

            for (const key in response.meta) {
               if (!allowedMeta.includes(key)) {
                  delete response.meta[key];
               }
            }
         }

         return response;
      } catch (e: any) {
         if (appConfig.is('nodeEnv', 'development')) {
            console.debug(e);
         }

         if (options?.noEmitEvent !== true) {
            // Emit the response failure
            eventPayload.success = false;
            eventPayload.responseData = e;

            try {
               await this.eventEmitter.emitAsync(eventConstant.onServiceResponse, eventPayload);
            } catch {}
         }

         throw e;
      }
   }

   createCRUD(patternCRUD: string, options?: ServiceOptions): CRUDClient {
      return {
         read: <TResult>(id: string, optionsOveride?: ServiceOptions): Promise<TResult> =>
            this.send(patternCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } }, optionsOveride ?? options),

         paginate: <TResult>(query?: PaginationQueryDto, optionsOveride?: ServiceOptions): Promise<TResult> =>
            this.send(patternCRUD, { meta: { query, CRUD: { method: 'read' } } }, optionsOveride ?? options),

         create: <TResult, TData>(data: TData, optionsOveride?: ServiceOptions): Promise<TResult> =>
            this.send(patternCRUD, { data, meta: { CRUD: { method: 'write' } } }, optionsOveride ?? options),

         update: <TResult, TData>(id: string, data: TData, optionsOveride?: ServiceOptions): Promise<TResult> =>
            this.send(
               patternCRUD,
               { data, meta: { params: { id }, CRUD: { method: 'write' } } },
               optionsOveride ?? options,
            ),

         delete: <TResult>(id: string, optionsOveride?: ServiceOptions): Promise<TResult> =>
            this.send(patternCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } }, optionsOveride ?? options),
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
