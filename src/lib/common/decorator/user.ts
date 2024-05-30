import { ExecutionContext, UnauthorizedException, createParamDecorator } from '@nestjs/common';
import { HttpRequest, MessageMeta } from '../type';
import { UserEntity } from '@lib/service/user/entity/user';
import { Is, Registry } from '@mvanvu/ujs';
import { metadata } from '@lib/metadata';

export const GetUser = createParamDecorator(
   (
      property: string | string[] | undefined | { optional?: boolean; property?: string | string[] },
      ctx: ExecutionContext,
   ) => {
      let user: UserEntity;

      if (metadata.isGateway()) {
         const { registry } = ctx.switchToHttp().getRequest<HttpRequest>();
         user = registry.get('user');
      } else {
         const {
            properties: { headers },
         } = ctx.switchToRpc().getContext().getMessage();

         user = Registry.from<MessageMeta>(headers?.['x-meta']).get('headers.user');
      }

      const isOptional = typeof property === 'object' && !Array.isArray(property) && property?.optional === true;

      if (!user) {
         if (!isOptional) {
            throw new UnauthorizedException();
         }

         return null;
      }

      if (!(user instanceof UserEntity)) {
         user = new UserEntity(UserEntity);
      }

      if (Is.string(property, true)) {
         return (property as string[]).map((prop) => user[prop]);
      }

      return typeof property === 'string' ? user[property] : user;
   },
);
