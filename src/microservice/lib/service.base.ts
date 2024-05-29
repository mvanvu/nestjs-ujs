import { MessageMeta } from '@lib/common';
import { UserEntity } from '@lib/service';
import { Path, Registry } from '@mvanvu/ujs';
import { Inject, Injectable, Scope } from '@nestjs/common';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

type MetaPath = Path<MessageMeta>;

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(CONTEXT) readonly ctx: RequestContext;

   get meta(): Registry<MetaPath> {
      return BaseService.parseMeta(this.ctx);
   }

   static parseMeta(ctx: RequestContext): Registry<MetaPath> {
      const {
         properties: { headers },
      } = ctx.getContext().getMessage();

      const meta = Registry.from<Path<MetaPath>>(headers?.['x-meta']);

      if (meta.has('headers.user')) {
         meta.set('headers.user', new UserEntity(meta.get('headers.user')));
      }

      return meta;
   }
}
