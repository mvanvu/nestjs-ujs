import { appConfig } from '@metadata';
import { Registry } from '@mvanvu/ujs';
import { FactoryProvider } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';
import { BaseEntity, Language, MessageMeta, MessageMetaProvider, UserRefEntity } from '@shared-library';

export const createMetaProvider = (): FactoryProvider => {
   return {
      provide: MessageMetaProvider,
      useFactory: (ctx: RequestContext) => {
         const {
            properties: { headers },
         } = ctx.getContext().getMessage();

         const meta = Registry.from<MessageMeta>(headers?.['x-meta']);
         meta.set('ctx', ctx);

         if (meta.has('user')) {
            meta.set('user', BaseEntity.bindToClass(meta.get('user'), UserRefEntity));
         }

         meta.set('language', new Language(meta.get('query.lang') || appConfig.get('language.default')));

         return meta;
      },
      inject: [CONTEXT],
   };
};
