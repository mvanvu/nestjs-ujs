import { Property } from '@lib/common/decorator/property';
import { AvailableStatus } from '.prisma/order';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class ItemToppingDto {
   @Property({
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   name: string;

   @Property({ validate: { is: 'uInt' } })
   price: number;
}

export class CreateItemDto {
   @Property({ validate: { is: 'mongoId' } })
   restaurantId: string;

   @Property({ validate: { is: 'mongoId' } })
   categoryId: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values(AvailableStatus) },
      swagger: { enum: AvailableStatus },
   })
   status?: AvailableStatus;

   @Property({
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   name: string;

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   imageUrl?: string;

   @Property({ validate: { is: 'uInt' } })
   basePrice: number;

   @Property({ optional: true, validate: { is: [ItemToppingDto] }, swagger: { type: [ItemToppingDto] } })
   toppings?: ItemToppingDto[];
}

export class UpdateItemDto extends IPartialType(CreateItemDto) {}
