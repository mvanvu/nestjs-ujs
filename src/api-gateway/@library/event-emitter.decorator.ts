import { eventConstant } from '@shared-library';
import { SetMetadata } from '@nestjs/common';

export const OnEvent = (event: string | string[]) => SetMetadata(eventConstant.metadataKey, event);
