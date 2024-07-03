import { StringSchema } from '@shared-library';

export class SendMailDto {
   @StringSchema({ optional: true, empty: false })
   from?: string;

   @StringSchema({ empty: false, isArray: 'unique', format: 'email' })
   to: string[];

   @StringSchema({ empty: false })
   subject: string;

   @StringSchema({ empty: false, transform: 'safeHtml' })
   body: string;
}
