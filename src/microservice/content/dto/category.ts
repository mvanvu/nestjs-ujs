import { Property, IPartialType } from '@shared-library';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';

export class CreateCategoryDto {
   @Property({ optional: true, validate: { is: 'mongoId' } })
   parentId?: string;

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

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   description?: string;

   @Property({ optional: true, validate: { is: MetadataDto }, swagger: { type: MetadataDto } })
   metadata?: MetadataDto;
}

export class UpdateCategoryDto extends IPartialType(CreateCategoryDto) {}
