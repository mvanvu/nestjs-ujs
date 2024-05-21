import { MessageMeta } from '@lib/common';
import { UserEntity } from '@lib/service';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   get meta(): Registry<MessageMeta> {
      const {
         properties: { headers },
      } = this.ctx.getContext().getMessage();

      const meta = Registry.from<MessageMeta>(headers?.['x-meta']);

      if (meta.has('headers.user')) {
         meta.set('headers.user', new UserEntity(meta.get('headers.user')));
      }

      return meta;
   }
}
