import { HTTP_CACHE_KEY, HttpCacheOptions } from '@shared-library';
import { SetMetadata } from '@nestjs/common';

export const HttpCache = (options: HttpCacheOptions) => SetMetadata(HTTP_CACHE_KEY, options);
