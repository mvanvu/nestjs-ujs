import { metadata } from '@lib/metadata';
import { Callable, Util } from '@mvanvu/ujs';
import { Injectable } from '@nestjs/common';

@Injectable()
export class BaseController {
   initGateway<TResult = any>(handler: Callable): Promise<TResult> {
      if (metadata.isGateway()) {
         return Util.callAsync(this, handler);
      }
   }

   initService<TResult = any>(handler: Callable): Promise<TResult> {
      if (metadata.isMicroservice()) {
         return Util.callAsync(this, handler);
      }
   }
}
