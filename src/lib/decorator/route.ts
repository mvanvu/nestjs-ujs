import { IRouteOptions } from '@lib/type';
import { Is } from '@mvanvu/ujs';
import { HttpCode, HttpStatus, RequestMethod, Version } from '@nestjs/common';
import { Delete, Get, Patch, Post, applyDecorators } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { ApiBearerAuth, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { Public } from './method';
import { metadata } from '@lib/metadata';

export function IRoute(options: IRouteOptions): MethodDecorator {
   const { pattern, route, swagger } = options;

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

   if (swagger) {
      decorators.push(ApiOperation({ summary: swagger.summary }));

      if (swagger.summary) {
         decorators.push(ApiOperation({ summary: swagger.summary }));
      }

      if (route?.httpStatus || swagger.responseType) {
         decorators.push(ApiResponse({ status: route?.httpStatus || HttpStatus.OK, type: swagger.responseType }));
      }
   }

   return decorators.length ? applyDecorators(...decorators) : () => {};
}
