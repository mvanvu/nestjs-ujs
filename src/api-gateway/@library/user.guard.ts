import {
   BaseEntity,
   DataMetaResult,
   HttpRequest,
   PermissionOptions,
   USER_PUBLIC_KEY,
   USER_ROLE_KEY,
   UserEntity,
} from '@shared-library';
import { CanActivate, ExecutionContext, ForbiddenException, Inject, Injectable } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { lastValueFrom, timeout } from 'rxjs';
import { injectProxy, serviceConfig, app as getApplication, appConfig } from '@metadata';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { Hash, Schema } from '@mvanvu/ujs';

@Injectable()
export class UserAuthGuard implements CanActivate {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   async canActivate(context: ExecutionContext): Promise<boolean> {
      const app = getApplication();
      const reflector = app.get(Reflector);
      const isPublic = reflector.getAllAndOverride<boolean>(USER_PUBLIC_KEY, [
         context.getHandler(),
         context.getClass(),
      ]);

      const request = context.switchToHttp().getRequest<HttpRequest>();
      const token = this.extractTokenFromHeader(request);

      if (!token) {
         if (isPublic) {
            return true;
         }

         throw new ForbiddenException();
      }

      try {
         const decode = Hash.jwt().decode(token);
         const userId = decode?.payload?.data?.id;

         if (Schema.mongoId().check(userId)) {
            const cacheKey = `${userId}:users/verify-token`;
            let user: UserEntity = await this.cacheManager.get(cacheKey);

            if (!user) {
               const response = await lastValueFrom<DataMetaResult<UserEntity>>(
                  app
                     .get<ClientProxy>(injectProxy('user'))
                     .send(serviceConfig.get('user.patterns.verifyToken'), { token })
                     .pipe(timeout(appConfig.get('apiGateway.requestTimeout'))),
               );

               user = response.data;

               if (user.id === userId) {
                  await this.cacheManager.set(cacheKey, user, appConfig.get('cache.ttl'));
               }
            }

            request.user = BaseEntity.bindToClass(user, UserEntity);
         } else {
            throw new ForbiddenException();
         }
      } catch (e) {
         if (isPublic) {
            return true;
         }

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
      const app = getApplication();
      const reflector = app.get(Reflector);
      const permission = reflector.getAllAndOverride<PermissionOptions | undefined>(USER_ROLE_KEY, [
         context.getHandler(),
         context.getClass(),
      ]);

      if (permission === undefined) {
         return true;
      }

      const { user } = context.switchToHttp().getRequest<HttpRequest>();

      if (!user?.authorise(permission)) {
         throw new ForbiddenException();
      }

      return true;
   }
}
