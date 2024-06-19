import { OnEvent } from '@gateway/lib';
import { OnServiceResponse, eventConstant } from '@lib/common';
import { ActivityLogDto } from '@lib/microservice/system';
import { injectProxy, serviceConfig } from '@metadata';
import { Is, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';

@Injectable()
export class ActivityLogProvider {
   @Inject(injectProxy(serviceConfig.get('system.name')))
   private readonly clientProxy: ClientProxy;

   private hideSecret(data: any, secretDeep?: boolean): void {
      const secretKeys: string[] = ['secret', 'tokens', 'token', 'hash', 'password'];

      if (Is.array(data)) {
         for (const datum of data) {
            this.hideSecret(datum);
         }
      } else if (Is.object(data)) {
         for (const k in data) {
            if (Is.objectOrArray(data[k])) {
               this.hideSecret(data[k], secretKeys.includes(k));
            } else if (Is.primitive(data[k]) && (secretKeys.includes(k) || secretDeep)) {
               data[k] = '******';
            }
         }
      }
   }

   @OnEvent(eventConstant.onServiceResponse)
   onServiceResponse(payload: OnServiceResponse): void {
      if (payload.httpRequest.method === 'GET') {
         return;
      }

      const { registry } = payload.httpRequest;
      const user = registry.get('user');
      const data: ActivityLogDto = {
         success: payload.success,
         messagePattern: payload.messagePattern,
         dataInput: { origin: Util.clone(payload.requestData ?? null) },
         dataResult: { origin: Util.clone(payload.responseData ?? null) },
         author: user ? { id: user.id, name: user.name, username: user.username, email: user.email } : null,
         ipAddress: registry.get('ipAddress', null),
         deviceType: registry.get('deviceType', null),
         userAgent: registry.get('userAgent', null),
         deviceOS: registry.get('deviceOS', null),
      };

      if (
         payload.messagePattern === serviceConfig.get('user.patterns.signIn') &&
         data.success &&
         data.dataResult.origin.user
      ) {
         const { user } = data.dataResult.origin;

         if (data.success && user) {
            data.author = { id: user.id, name: user.name, username: user.username, email: user.email };
         }
      }

      this.hideSecret(data.dataInput);
      this.hideSecret(data.dataResult);
      this.clientProxy.emit(serviceConfig.get('system.patterns.writeActivityLog'), data);
   }
}
