import { PermissionOptions, USER_PUBLIC_KEY, USER_ROLE_KEY } from '@lib';
import { SetMetadata } from '@nestjs/common';

export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);
export const Permission = (options?: PermissionOptions) => SetMetadata(USER_ROLE_KEY, options ?? {});
