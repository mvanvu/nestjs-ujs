import { EnumSchema, IPartialType, NameSchema } from '@shared-library';
import { AvailableStatus } from '.prisma/content';

export class CreateTagDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NameSchema()
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
