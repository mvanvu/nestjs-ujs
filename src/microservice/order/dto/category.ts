import { AvailableStatus } from '.prisma/order';
import { IPartialType, Property } from '@shared-library';

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
