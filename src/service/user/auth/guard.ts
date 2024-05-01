import { metadata, HttpRequest, UserPermission, USER_PUBLIC_KEY, USER_ROLE_KEY, UserRole } from '@lib';
import { Is } from '@mvanvu/ujs';
import { CanActivate, ExecutionContext, ForbiddenException, Injectable, UnauthorizedException } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { userConfig } from '@service/user/user.config';
import { lastValueFrom, timeout } from 'rxjs';

@Injectable()
export class UserAuthGuard implements CanActivate {
   async canActivate(context: ExecutionContext): Promise<boolean> {
      if (metadata.isMicroservice()) {
         return true;
      }

      const app = metadata.getGateway();
      const reflector = app.get(Reflector);
      const isPublic = reflector.getAllAndOverride<boolean>(USER_PUBLIC_KEY, [
         context.getHandler(),
         context.getClass(),
      ]);

      if (isPublic) {
         return true;
      }

      const request = context.switchToHttp().getRequest<HttpRequest>();
      const token = this.extractTokenFromHeader(request);

      if (!token) {
         throw new ForbiddenException();
      }

      try {
         const user = await lastValueFrom(
            app.get<ClientProxy>(userConfig.proxy).send(userConfig.patterns.verify, token).pipe(timeout(5000)),
         );

         request.registry.set('user', user);
      } catch (e) {
         throw new ForbiddenException();
      }

      return true;
   }

   private extractTokenFromHeader(request: HttpRequest): string | undefined {
      const [type, token] = request.headers.authorization?.split(' ') ?? [];
      return type === 'Bearer' ? token : undefined;
   }
}

@Injectable()
export class UserRoleGuard implements CanActivate {
   async canActivate(context: ExecutionContext): Promise<boolean> {
      if (metadata.isMicroservice()) {
         return true;
      }

      const app = metadata.getGateway();
      const reflector = app.get(Reflector);
      const permission = reflector.getAllAndOverride<UserPermission & { root?: boolean }>(USER_ROLE_KEY, [
         context.getHandler(),
         context.getClass(),
      ]);

      if (!permission) {
         return true;
      }

      const { registry } = context.switchToHttp().getRequest<HttpRequest>();

      if (registry) {
         const roles = registry.get<UserRole>('user.roles', []);
         const isUserRoot = !!roles.find(({ root }) => root === true);

         if (permission.root === true && !isUserRoot) {
            throw new UnauthorizedException();
         }

         if (isUserRoot) {
            return true;
         }

         for (const role of roles) {
            const permissionRole = role.permissions.find(({ refModel }) => refModel === permission.refModel);

            if (!permissionRole) {
               continue;
            }

            const { canRead, canCreate, canUpdate, canDelete } = permissionRole;

            if (Is.nullOrUndefined([canRead, canCreate, canUpdate, canDelete], true)) {
               return true;
            }

            const permitRecord: Record<'canRead' | 'canCreate' | 'canUpdate' | 'canDelete', boolean> = {
               canRead,
               canCreate,
               canUpdate,
               canDelete,
            };

            let passed: boolean = true;

            for (const key in permitRecord) {
               if (
                  Is.boolean(permitRecord[key]) &&
                  !role.permissions.find((permit) => permit[key] === permitRecord[key])
               ) {
                  passed = false;
                  break;
               }
            }

            if (passed) {
               return true;
            }
         }
      }

      throw new UnauthorizedException();
   }
}
