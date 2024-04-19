// import { USER_PUBLIC_KEY } from '@api-gateway/lib/decorator';
// import { HttpRequest } from '@api-gateway/lib/type';
// import {
//    CanActivate,
//    ExecutionContext,
//    HttpException,
//    Inject,
//    Injectable,
//    UnauthorizedException,
// } from '@nestjs/common';
// import { Reflector } from '@nestjs/core';
// import { ClientProxy } from '@nestjs/microservices';
// import { appConstant } from '@share-lib';
// import { lastValueFrom } from 'rxjs';

// @Injectable()
// export class UserGuard implements CanActivate {
//    constructor(
//       @Inject(appConstant.user.name) protected readonly client: ClientProxy,
//       private readonly reflector: Reflector,
//    ) {}

//    async canActivate(context: ExecutionContext) {
//       const isPublic = this.reflector.getAllAndOverride<boolean>(USER_PUBLIC_KEY, [
//          context.getHandler(),
//          context.getClass(),
//       ]);

//       if (isPublic) {
//          return true;
//       }

//       const request = context.switchToHttp().getRequest<HttpRequest>();
//       const token = this.extractTokenFromHeader(request);

//       if (!token) {
//          throw new UnauthorizedException();
//       }

//       try {
//          const { data: user } = await lastValueFrom(
//             this.client.send(appConstant.user.patterns.validateToken, { token, type: 'access' }),
//          );
//          request.registry.set('user', user);

//          return true;
//       } catch (e: any) {
//          const { statusCode, message, error } = e;
//          throw new HttpException({ message, error }, statusCode);
//       }
//    }

//    private extractTokenFromHeader(request: HttpRequest): string | undefined {
//       const [type, token] = request.headers.authorization?.split(' ') ?? [];
//       return type === 'Bearer' ? token : undefined;
//    }
// }
