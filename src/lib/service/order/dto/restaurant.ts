import { Property } from '@lib/common/decorator/property';
import { AvailableStatus } from '.prisma/content';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class CreateRestaurantDto {
   @Property({ validate: { is: 'mongoId' } })
   ownerId: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values(AvailableStatus) },
      swagger: { enum: AvailableStatus },
   })
   status?: AvailableStatus;

   @Property({
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: 'trim' },
   })
   name: string;

   @Property({
      optional: true,
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: ['toSafeHtml', 'trim'] },
   })
   description?: string;

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   imageUrl?: string;

   @Property({
      optional: true,
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   address?: string;

   @Property({
      optional: true,
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   phoneNumber?: string;

   @Property({ optional: true, validate: { is: 'email' } })
   email?: string;
}

export class UpdateRestaurantDto extends IPartialType(CreateRestaurantDto) {}
