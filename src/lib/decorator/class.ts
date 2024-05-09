import { metadata } from '@lib/metadata';
import { ClassConstructor } from '@lib/type';

export const DTO_FOR = '__DTO_FOR__';

export function IDto<T>(appEnv?: 'gateway' | 'service') {
   return (target: ClassConstructor<T>) => {
      target.prototype[DTO_FOR] = appEnv ?? (metadata.isGateway() ? 'gateway' : 'service');
   };
}
