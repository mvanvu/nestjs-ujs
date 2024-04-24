import { appConfig } from '@lib/core/config';
import { metadata } from '@lib/metadata';
import { Is } from '@mvanvu/ujs';
import { ExceptionFilter as NestExceptionFilter, Catch, ArgumentsHost, HttpStatus } from '@nestjs/common';
import { RpcException } from '@nestjs/microservices';
import { Response } from 'express';
import { Observable, throwError } from 'rxjs';

@Catch()
export class ExceptionFilter implements NestExceptionFilter {
   catch(exception: any, host: ArgumentsHost): void | Observable<any> {
      if (metadata.isGateway()) {
         const response = host.switchToHttp().getResponse<Response>();
         const status = Is.func(exception?.getStatus) ? exception.getStatus() : HttpStatus.BAD_GATEWAY;
         const exceptionResponse = Is.func(exception?.getResponse) ? exception.getResponse() : exception;
         const error =
            exceptionResponse?.error ||
            exceptionResponse?.stack ||
            exceptionResponse?.detail ||
            exceptionResponse?.message;
         const jsonRes: {
            success: boolean;
            error: any;
            stack?: string;
         } = {
            success: false,
            error,
         };

         if (appConfig.nodeEnv !== 'production') {
            jsonRes.stack = exception.stack;
         }

         response.status(status).json(jsonRes);
      } else {
         return throwError(() =>
            exception instanceof RpcException
               ? exception.getError()
               : {
                    code: HttpStatus.BAD_GATEWAY,
                    error: exception?.stack || exception?.message,
                 },
         );
      }
   }
}
