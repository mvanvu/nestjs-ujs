import { OnServiceResponse, UserRefEntity, detectDevice, eventConstant, snackToCamelCase } from '@shared-library';
import { injectProxy, serviceConfig } from '@metadata';
import { Is, Util } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';
import { OnEvent } from '@gateway/@library/event-emitter.decorator';
import { ActivityLogDto } from '@microservice/system/dto';
import { Prisma } from '.prisma/system';

@Injectable()
export class ActivityLogProvider {
   @Inject(injectProxy(serviceConfig.get('system.name')))
   private readonly clientProxy: ClientProxy;

   private hideSecret(data: any, secretDeep?: boolean): void {
      const secretKeys: string[] = ['secret', 'token', 'pass', 'hash', 'password', 'cardNumber', 'cvc', 'cvv'];

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
   async onServiceResponse({ success, messagePattern, httpRequest, requestData, responseData }: OnServiceResponse) {
      if (httpRequest.method === 'GET') {
         return;
      }

      const userAgent = httpRequest.headers['user-agent'] || '';
      const userRef = requestData?.meta?.user ? Util.clone(requestData.meta.user) : null;
      const data: ActivityLogDto = {
         success,
         messagePattern,
         dataInput: { origin: Util.clone(requestData ?? null) },
         dataResult: { origin: Util.clone(responseData ?? null) },
         author: userRef,
         userAgent,
         ipAddress: httpRequest.ips.length ? httpRequest.ips[0] : httpRequest.ip,
         detectResult: snackToCamelCase<Prisma.InputJsonObject>(detectDevice(userAgent)),
      };

      if (
         messagePattern === serviceConfig.get('user.patterns.signIn') &&
         success &&
         data.dataResult.origin?.data.user
      ) {
         data.author = new UserRefEntity(data.dataResult.origin.user);
      }

      if (data.dataInput.origin?.meta?.user) {
         delete data.dataInput.origin.meta.user;
      }

      this.hideSecret(data.dataInput);
      this.hideSecret(data.dataResult);
      this.clientProxy.emit(serviceConfig.get('system.patterns.writeActivityLog'), data);
   }
}
