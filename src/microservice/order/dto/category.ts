import { AvailableStatus } from '.prisma/order';
import { IPartialType, IsIn, IsString } from '@shared-library';

export class CreateCategoryDto {
   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsString({ notEmpty: true })
   name: string;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
