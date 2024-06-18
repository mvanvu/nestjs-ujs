import { isGateway } from '@metadata';
import { ObjectRecord } from '@mvanvu/ujs';
import { HttpException, HttpStatus } from '@nestjs/common';
import { RpcException } from '@nestjs/microservices';

export function ThrowException(error: string | ObjectRecord, statusCode?: HttpStatus) {
   if (isGateway()) {
      throw new HttpException({ error }, statusCode ?? HttpStatus.BAD_REQUEST);
   } else {
      throw new RpcException({ error, statusCode });
   }
}
