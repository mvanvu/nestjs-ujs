import { Property } from '@lib/common/decorator/property';
import { AvailableStatus } from '.prisma/order';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class CreateTableDto {
   @Property({ validate: { is: 'mongoId' } })
   restaurantId: string;

   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values(AvailableStatus) },
      swagger: { enum: AvailableStatus },
   })
   status?: AvailableStatus;

   @Property({ validate: [{ is: 'uInt' }, { is: 'min', meta: 1 }] })
   number: number;

   @Property({
      optional: true,
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
   })
   area?: string;
}

export class UpdateTableDto extends IPartialType(CreateTableDto) {}
