import { StringSchema } from '@shared-library';

export class SendMailDto {
   @StringSchema({ optional: true, notEmpty: true })
   from?: string;

   @StringSchema({ notEmpty: true, each: 'unique', format: 'email' })
   to: string[];

   @StringSchema({ notEmpty: true })
   subject: string;

   @StringSchema({ notEmpty: true, transform: 'safeHtml' })
   body: string;
}
