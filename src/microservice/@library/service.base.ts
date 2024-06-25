import { MessageMeta, UserEntity } from '@shared-library';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable, Scope } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';
import * as i18n from '@shared-library/i18n';

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   get meta(): Registry<MessageMeta> {
      return BaseService.parseMeta(this.ctx);
   }

   get user(): UserEntity | undefined {
      return this.meta.get('user');
   }

   get language(): i18n.Language {
      return new i18n.Language(this.meta.get('query.lang'));
   }

   static parseMeta(ctx: RequestContext): Registry<MessageMeta> {
      const {
         properties: { headers },
      } = ctx.getContext().getMessage();

      const meta = Registry.from<MessageMeta>(headers?.['x-meta']);

      if (meta.has('user')) {
         meta.set('user', new UserEntity(meta.get('user')));
      }

      return meta;
   }
}
