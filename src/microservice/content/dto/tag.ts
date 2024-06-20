import { Property, IPartialType } from '@shared-library';
import { AvailableStatus } from '.prisma/content';

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
