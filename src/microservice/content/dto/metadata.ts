import { IsString } from '@shared-library';

export class MetadataDto {
   @IsString({ optional: true })
   title?: string;

   @IsString({ optional: true })
   description?: string;

   @IsString({ optional: true, url: true })
   image?: string;

   @IsString({ optional: true })
   author?: string;

   @IsString({ optional: true })
   robot?: string;

   @IsString({ optional: true })
   index?: string;
}
