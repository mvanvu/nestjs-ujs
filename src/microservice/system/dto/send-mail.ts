import { IsArray, IsString } from '@shared-library';
export class SendMailDto {
   @IsString({ optional: true, notEmpty: true })
   from?: string;

   @IsArray({ notEmpty: true, unique: true })
   to: string[];

   @IsString({ notEmpty: true })
   subject: string;

   @IsString({ notEmpty: true, safeHtml: true })
   body: string;
}
