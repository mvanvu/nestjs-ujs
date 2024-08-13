import { HttpRequest, PermissionOptions, USER_PUBLIC_KEY, USER_ROLE_KEY } from '@shared-library';
import { ExecutionContext, SetMetadata, UnauthorizedException, createParamDecorator } from '@nestjs/common';
import { Is } from '@mvanvu/ujs';

export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);
export const Permission = (options?: PermissionOptions) => SetMetadata(USER_ROLE_KEY, options ?? {});
export const User = createParamDecorator(
   (property: string | undefined | { optional?: boolean; property?: string }, ctx: ExecutionContext) => {
      const { user } = ctx.switchToHttp().getRequest<HttpRequest>();
      const isOptional = typeof property === 'object' && !Is.array(property) && property?.optional === true;

      if (!user) {
         if (!isOptional) {
            throw new UnauthorizedException();
         }

         return null;
      }

      return Is.string(property) ? user[property] : user;
   },
);
