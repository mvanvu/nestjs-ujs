import { SetMetadata } from '@nestjs/common';
export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const UserPublic = () => SetMetadata(USER_PUBLIC_KEY, true);
