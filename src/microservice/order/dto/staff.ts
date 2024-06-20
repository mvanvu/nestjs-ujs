import { StaffStatus } from '.prisma/order';
import { IPartialType, Property } from '@shared-library';

export class CreateStaffDto {
   @Property({ validate: { is: 'mongoId' } })
   restaurantId: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values(StaffStatus) },
      swagger: { enum: StaffStatus },
   })
   status?: StaffStatus;

   @Property({
      optional: true,
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   name: string;

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   imageUrl?: string;

   @Property({
      optional: true,
      validate: { is: 'string' },
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   phoneNumber?: string;

   @Property({ optional: true, validate: { is: 'email' } })
   email?: string;
}

export class UpdateStaffDto extends IPartialType(CreateStaffDto) {}
