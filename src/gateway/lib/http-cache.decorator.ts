import { HTTP_CACHE_KEY, ICacheOptions } from '@lib/common';
import { SetMetadata } from '@nestjs/common';

export const ICache = (options: ICacheOptions) => {
   return SetMetadata(HTTP_CACHE_KEY, {
      disabled: options.disabled === true,
      withUserIdPrefix: options.withUserIdPrefix === true,
   });
};
