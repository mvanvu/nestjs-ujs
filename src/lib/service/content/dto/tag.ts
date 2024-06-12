import { Property } from '@lib/common/decorator/property';
import { AvailableStatus } from '.prisma/content';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class CreateTagDto {
   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.values(AvailableStatus) },
      swagger: { enum: AvailableStatus },
   })
   status?: AvailableStatus;

   @Property({
      optional: true,
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: 'trim' },
   })
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
