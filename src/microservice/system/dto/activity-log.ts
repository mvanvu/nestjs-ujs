import { Prisma } from '.prisma/system';
import { IsBoolean, IsDTO, IsJsonObject, IsMongoId, IsString } from '@shared-library';

export class ActivityLogAuthor {
   @IsMongoId()
   id: string;

   @IsString({ optional: true })
   name?: string;

   @IsString({ optional: true })
   username?: string;

   @IsString({ optional: true, email: true })
   email?: string;
}

export class ActivityLogDto {
   @IsBoolean()
   success: boolean;

   @IsString({ notEmpty: true })
   messagePattern: string;

   @IsJsonObject({ optional: true, swagger: { disabled: true } })
   dataInput?: { origin: any };

   @IsJsonObject({ optional: true, swagger: { disabled: true } })
   dataResult?: { origin: any };

   @IsDTO(ActivityLogAuthor, { optional: true })
   author?: ActivityLogAuthor;

   @IsString({ optional: true })
   ipAddress?: string;

   @IsString({ optional: true })
   userAgent?: string;

   @IsJsonObject({ optional: true, swagger: { disabled: true } })
   detectResult?: Prisma.InputJsonObject;
}
