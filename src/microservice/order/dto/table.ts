import { AvailableStatus } from '.prisma/order';
import { IPartialType, IsIn, IsMongoId, IsNumber, IsString } from '@shared-library';

export class CreateTableDto {
   @IsMongoId()
   restaurantId: string;

   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsNumber({ unsigned: true, min: 1 })
   number: number;

   @IsString({ optional: true, notEmpty: true })
   area?: string;
}

export class UpdateTableDto extends IPartialType(CreateTableDto) {}
