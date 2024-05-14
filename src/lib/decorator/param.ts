import { HttpRequest } from '@lib/type';
import { Is, Registry } from '@mvanvu/ujs';
import { createParamDecorator, ExecutionContext, UnauthorizedException } from '@nestjs/common';

export const GetUser = createParamDecorator(
   (
      property: string | string[] | undefined | { optional?: boolean; property?: string | string[] },
      ctx: ExecutionContext,
   ) => {
      const { registry } = ctx.switchToHttp().getRequest<HttpRequest>();
      const isOptional = typeof property === 'object' && !Array.isArray(property) && property?.optional === true;
      const user = registry.get('user');

      if (!user) {
         if (!isOptional) {
            throw new UnauthorizedException();
         }

         return null;
      }

      const reg = Registry.from(user);

      if (Is.string(property, true)) {
         return (property as string[]).map((prop) => reg.get(prop));
      }

      return typeof property === 'string' ? reg.get(property) : reg.valueOf();
   },
);
