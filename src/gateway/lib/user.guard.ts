import { metadata, HttpRequest, PermissionOptions } from '@lib/common';
import { CanActivate, ExecutionContext, ForbiddenException, Injectable } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { UserEntity } from '@lib/service/user';
import { lastValueFrom, timeout } from 'rxjs';
import { serviceConfig } from '@config';
import { USER_PUBLIC_KEY, USER_ROLE_KEY } from './user.decorator';

@Injectable()
export class UserAuthGuard implements CanActivate {
   async canActivate(context: ExecutionContext): Promise<boolean> {
      const app = metadata.getGateway();
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
         const user = await lastValueFrom(
            app
               .get<ClientProxy>(serviceConfig.get<string>('user.name').toUpperCase() + '_MICROSERVICE')
               .send(serviceConfig.get('user.patterns.verify'), token)
               .pipe(timeout(5000)),
         );

         request.registry.set('user', new UserEntity(user));
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
      const app = metadata.getGateway();
      const reflector = app.get(Reflector);
      const permission = reflector.getAllAndOverride<PermissionOptions | undefined>(USER_ROLE_KEY, [
         context.getHandler(),
         context.getClass(),
      ]);

      if (permission === undefined) {
         return true;
      }

      const { registry } = context.switchToHttp().getRequest<HttpRequest>();
      const user = registry.get('user');

      if (!user?.authorise(permission)) {
         throw new ForbiddenException();
      }

      return true;
   }
}
