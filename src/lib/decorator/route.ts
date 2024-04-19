import { metadata } from '@/core';
import { Delete, Get, Patch, Post, applyDecorators } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';

export function IRoute(options: {
   pattern: string;
   route?: { method: 'POST' | 'PATCH' | 'GET' | 'DELETE'; path: string | string[] };
}): MethodDecorator {
   if (metadata.isMicroservice()) {
      return applyDecorators(MessagePattern(options.pattern));
   }

   if (options.route) {
      switch (options.route.method) {
         case 'GET':
            return applyDecorators(Get(options.route.path));

         case 'POST':
            return applyDecorators(Post(options.route.path));

         case 'PATCH':
            return applyDecorators(Patch(options.route.path));

         case 'DELETE':
            return applyDecorators(Delete(options.route.path));
      }
   }

   return () => {};
}
