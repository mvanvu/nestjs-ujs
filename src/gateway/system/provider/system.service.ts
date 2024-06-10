import { OnEvent } from '@gateway/lib';
import { OnServiceResponse, eventConstant } from '@lib/common';
import { ActivityLogDto } from '@lib/service/system';
import { serviceConfig } from '@metadata';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';

@Injectable()
export class SystemService {
   @Inject(serviceConfig.get('system.name').toUpperCase() + '_MICROSERVICE')
   private readonly clientProxy: ClientProxy;

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
         dataInput: { origin: payload.requestData ?? null },
         dataResult: { origin: payload.responseData ?? null },
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

      this.clientProxy.emit(serviceConfig.get('system.patterns.writeActivityLog'), data);
   }
}
