import { metadata } from '@lib/metadata';
import { HttpRequest } from '@lib/type';
import { Is, Registry } from '@mvanvu/ujs';
import {
   Body,
   createParamDecorator,
   ExecutionContext,
   NestInterceptor,
   Param,
   PipeTransform,
   Query,
   Type,
   UnauthorizedException,
   UseInterceptors,
} from '@nestjs/common';
import { Payload } from '@nestjs/microservices';

export const GetUser = createParamDecorator(
   (
      property: string | string[] | undefined | { optional?: boolean; property?: string | string[] },
      ctx: ExecutionContext,
   ) => {
      const { registry } = ctx.switchToHttp().getRequest<HttpRequest>();
      const isOptional = typeof property === 'object' && !Array.isArray(property) && property?.optional === true;
      const user = registry.get('user');

      if (!user) {
         if (!isOptional) {
            throw new UnauthorizedException();
         }

         return null;
      }

      const reg = Registry.from(user);

      if (Is.string(property, true)) {
         return (property as string[]).map((prop) => reg.get(prop));
      }

      return typeof property === 'string' ? reg.get(property) : reg.valueOf();
   },
);

export function IPayload(...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isGateway() ? Body(...pipes) : Payload(...pipes);
}

export function IGatewayPayload(...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isGateway() ? Body(...pipes) : () => {};
}

export function IServicePayload(...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isMicroservice() ? Payload(...pipes) : () => {};
}

export function IGatewayInterceptors(
   ...interceptors: (NestInterceptor | Function)[]
): MethodDecorator & ClassDecorator {
   return metadata.isGateway() ? UseInterceptors(...interceptors) : () => {};
}

export function IQuery(...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isGateway() ? Query(...pipes) : Payload(...pipes);
}

export function IParam(property?: string, ...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isGateway() ? Param(property, ...pipes) : Payload(property, ...pipes);
}
