import { Property } from '@lib/common/decorator/property';
import { AvailableStatus } from '.prisma/order';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class CreateCategoryDto {
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
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
