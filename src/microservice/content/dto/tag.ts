import { IPartialType, IsIn, IsString } from '@shared-library';
import { AvailableStatus } from '.prisma/content';

export class CreateTagDto {
   @IsIn(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @IsString({ notEmpty: true })
   name: string;
}

export class UpdateTagDto extends IPartialType(CreateTagDto) {}
