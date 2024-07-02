import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, StringSchema } from '@shared-library';

export class CreateCategoryDto {
   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @StringSchema({ notEmpty: true })
   name: string;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
