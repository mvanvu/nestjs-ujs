import { isGateway } from '@metadata';
import { ObjectRecord } from '@mvanvu/ujs';
import { HttpException, HttpStatus } from '@nestjs/common';
import { RpcException } from '@nestjs/microservices';

export function ThrowException(error: string | ObjectRecord, statusCode?: HttpStatus) {
   statusCode = statusCode ?? HttpStatus.BAD_REQUEST;

   if (isGateway()) {
      throw new HttpException({ error }, statusCode ?? HttpStatus.BAD_REQUEST);
   } else {
      error = typeof error === 'string' ? { error } : error;

      if (error?.statusCode === undefined) {
         error.statusCode = statusCode ?? HttpStatus.BAD_REQUEST;
      }

      throw new RpcException(error);
   }
}
