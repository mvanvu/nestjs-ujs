import { MessageMeta, UserEntity } from '@shared-library';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable, Scope } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';
import * as i18n from '@shared-library/i18n';

export const parseContextMeta = (ctx: RequestContext): Registry<MessageMeta> => {
   const {
      properties: { headers },
   } = ctx.getContext().getMessage();

   const meta = Registry.from<MessageMeta>(headers?.['x-meta']);

   if (meta.has('user')) {
      meta.set('user', new UserEntity(meta.get('user')));
   }

   meta.set('language', new i18n.Language(meta.get('query.lang')));

   return meta;
};

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   private contextMeta: Registry<MessageMeta>;

   get meta(): Registry<MessageMeta> {
      if (!this.contextMeta) {
         this.contextMeta = parseContextMeta(this.ctx);
      }

      return this.contextMeta;
   }

   get user(): UserEntity | undefined {
      return this.meta.get('user');
   }

   get language(): i18n.Language {
      return new i18n.Language(this.meta.get('query.lang'));
   }
}
