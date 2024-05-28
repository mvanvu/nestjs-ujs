import { MessageMeta } from '@lib/common';
import { UserEntity } from '@lib/service';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable, Scope } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   get meta(): Registry<MessageMeta> {
      return BaseService.parseMeta(this.ctx);
   }

   static parseMeta(ctx: RequestContext): Registry<MessageMeta> {
      const {
         properties: { headers },
      } = ctx.getContext().getMessage();

      const meta = Registry.from<MessageMeta>(headers?.['x-meta']);

      if (meta.has('headers.user')) {
         meta.set('headers.user', new UserEntity(meta.get('headers.user')));
      }

      return meta;
   }
}
