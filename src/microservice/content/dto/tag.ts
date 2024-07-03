import { EnumSchema, IPartialType, StringSchema } from '@shared-library';
import { AvailableStatus } from '.prisma/content';

export class CreateTagDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ empty: false })
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
