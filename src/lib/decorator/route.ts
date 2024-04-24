import { IRouteOptions, metadata } from '@lib';
import { Is } from '@mvanvu/ujs';
import { HttpCode, RequestMethod, SetMetadata, Version } from '@nestjs/common';
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

   if (!Is.nullOrUndefined(route?.method) || !Is.nullOrUndefined(route?.httpStatus) || route?.version) {
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

      if (!Is.nullOrUndefined(route.httpStatus)) {
         decorators.push(HttpCode(route.httpStatus));
      }

      if (route.version) {
         decorators.push(Version(route.version));
      }

      decorators.push(route.public === true ? Public() : ApiBearerAuth());
   }

   return decorators.length ? applyDecorators(...decorators) : () => {};
}
