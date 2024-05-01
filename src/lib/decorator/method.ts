import { SetMetadata } from '@nestjs/common';

export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);

export const USER_ROLE_KEY = 'USER_ROLE_KEY';
export const Permission = (options: {
   refModel: string;
   root?: boolean;
   canRead?: boolean;
   canCreate?: boolean;
   canUpdate?: boolean;
   canDelete?: boolean;
}) => SetMetadata(USER_ROLE_KEY, options);
