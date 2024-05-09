import { PermissionOptions } from '@lib/type';
import { SetMetadata } from '@nestjs/common';

export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);

export const USER_ROLE_KEY = 'USER_ROLE_KEY';
export const Permission = (options?: PermissionOptions) => SetMetadata(USER_ROLE_KEY, options ?? {});
