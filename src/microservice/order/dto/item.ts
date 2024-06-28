import { AvailableStatus } from '.prisma/order';
import { IPartialType, IsDTO, IsIn, IsMongoId, IsNumber, IsString } from '@shared-library';

export class ItemToppingDto {
   @IsString({ notEmpty: true })
   name: string;

   @IsNumber({ unsigned: true })
   price: number;
}

export class CreateItemDto {
   @IsMongoId()
   restaurantId: string;

   @IsMongoId()
   categoryId: string;

   @IsIn(Object.values(AvailableStatus))
   status?: AvailableStatus;

   @IsString({ notEmpty: true })
   name: string;

   @IsString({ optional: true, url: true })
   imageUrl?: string;

   @IsNumber({ unsigned: true })
   basePrice: number;

   @IsDTO(ItemToppingDto, { optional: true, each: 'unique' })
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends IPartialType(CreateItemDto) {}
