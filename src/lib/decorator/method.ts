import { metadata } from '@lib/metadata';
import { NestInterceptor, SetMetadata, UseInterceptors } from '@nestjs/common';

export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);

export const USER_ROLE_KEY = 'USER_ROLE_KEY';
export const Permission = (options?: { key?: string; or?: string[]; and?: string[] }) =>
   SetMetadata(USER_ROLE_KEY, options ?? {});

export function IInterceptors(
   appEnv: 'gateway' | 'service',
   ...interceptors: (NestInterceptor | Function)[]
): MethodDecorator & ClassDecorator {
   return (metadata.isGateway() && appEnv === 'gateway') || (metadata.isMicroservice() && appEnv === 'service')
      ? UseInterceptors(...interceptors)
      : () => {};
}
