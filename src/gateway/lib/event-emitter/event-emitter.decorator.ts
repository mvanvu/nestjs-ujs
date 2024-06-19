import { eventConstant } from '@lib/common';
import { SetMetadata } from '@nestjs/common';

export const OnEvent = (event: string | string[]) => SetMetadata(eventConstant.metadataKey, event);
