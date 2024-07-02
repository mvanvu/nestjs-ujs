import { EnumSchema, IPartialType, StringSchema } from '@shared-library';
import { AvailableStatus } from '.prisma/content';

export class CreateTagDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ notEmpty: true })
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
