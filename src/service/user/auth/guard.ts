import { metadata } from '@lib';
import { USER_PUBLIC_KEY } from '@lib/decorator';
import { HttpRequest } from '@lib/type';
import { CanActivate, ExecutionContext, Injectable, UnauthorizedException } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { ClientProxy } from '@nestjs/microservices';
import { userConfig } from '@service/user/user.config';
import { lastValueFrom, timeout } from 'rxjs';

@Injectable()
export class UserAuthGuard implements CanActivate {
   async canActivate(context: ExecutionContext): Promise<boolean> {
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
         throw new UnauthorizedException();
      }

      try {
         const user = await lastValueFrom(
            app
               .get<ClientProxy>(userConfig.proxy)
               .send(userConfig.patterns.verify, { data: token })
               .pipe(timeout(5000)),
         );

         request.registry.set('user', user);
      } catch {
         throw new UnauthorizedException();
      }

      return true;
   }

   private extractTokenFromHeader(request: HttpRequest): string | undefined {
      const [type, token] = request.headers.authorization?.split(' ') ?? [];
      return type === 'Bearer' ? token : undefined;
   }
}
