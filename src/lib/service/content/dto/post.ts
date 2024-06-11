import { Property } from '@lib/common/decorator/property';
import { MetadataDto } from './metadata';
import { AvailableStatus } from '.prisma/content';
import { IPartialType } from '@lib/common/entity/mapped-type';

export class CreatePostDto {
   @Property({
      optional: true,
      validate: { is: 'inArray', meta: Object.keys(AvailableStatus) },
      swagger: { enum: AvailableStatus },
   })
   status?: AvailableStatus;

   @Property({ optional: true, validate: { is: 'mongoId' } })
   categoryId?: string;

   @Property({
      optional: true,
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      transform: { fromType: 'string', toType: 'trim' },
   })
   name: string;

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   description?: string;

   @Property({ optional: true, validate: { is: 'string' }, transform: { fromType: 'string', toType: 'trim' } })
   imageUrl?: string;

   @Property({ optional: true, validate: { is: MetadataDto }, swagger: { type: MetadataDto } })
   metadata?: MetadataDto;
}

export class UpdatePostDto extends IPartialType(CreatePostDto) {}
