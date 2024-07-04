import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, NameSchema } from '@shared-library';

export class CreateCategoryDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NameSchema()
   name: string;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
