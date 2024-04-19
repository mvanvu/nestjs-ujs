import { Controller, applyDecorators } from '@nestjs/common';

export function IController(path?: string | string[]) {
   if (process.env.APP_ENV !== 'gateway') {
      path = undefined;
   }

   return applyDecorators(Controller(path));
}
