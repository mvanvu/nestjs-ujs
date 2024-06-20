import { MessageMeta, UserEntity } from '@shared-library';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable, Scope } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   get meta(): Registry<MessageMeta> {
      return BaseService.parseMeta(this.ctx);
   }

   get user(): UserEntity | undefined {
      return this.meta.get('headers.user');
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
