import { USER_PUBLIC_KEY, USER_ROLE_KEY } from '@lib/common';
import { SetMetadata } from '@nestjs/common';

export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);
export const Permission = (options?: { key?: string; or?: string[]; and?: string[] }) =>
   SetMetadata(USER_ROLE_KEY, options ?? {});
