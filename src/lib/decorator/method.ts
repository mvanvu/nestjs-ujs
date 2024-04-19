import { IRouteOptions, metadata } from '@lib';
import { RequestMethod, SetMetadata, Version } from '@nestjs/common';
import { Delete, Get, Patch, Post, applyDecorators } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { ApiBearerAuth } from '@nestjs/swagger';

export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);

export function IRoute(options: IRouteOptions): MethodDecorator {
   const { pattern, route } = options;

   if (metadata.isMicroservice()) {
      return applyDecorators(MessagePattern(pattern));
   }

   const decorators: Array<ClassDecorator | MethodDecorator | PropertyDecorator> = [];

   if (route.path) {
      switch (route.method) {
         case RequestMethod.GET:
            decorators.push(Get(route.path));
            break;

         case RequestMethod.POST:
            decorators.push(Post(route.path));
            break;

         case RequestMethod.PATCH:
            decorators.push(Patch(route.path));
            break;

         case RequestMethod.DELETE:
            decorators.push(Delete(route.path));
            break;
      }
   }

   if (route.version !== undefined) {
      decorators.push(Version(route.version));
   }

   if (route.path || route.version) {
      decorators.push(route.public === true ? Public() : ApiBearerAuth());
   }

   return decorators.length ? applyDecorators(...decorators) : () => {};
}
