import { EmailSchema, StringSchema } from '@shared-library';

export class SendMailDto {
   @StringSchema({ optional: true, empty: false })
   from?: string;

   @EmailSchema({ isArray: 'unique' })
   to: string[];

   @StringSchema({ empty: false })
   subject: string;

   @StringSchema({ empty: false, transform: 'safeHtml' })
   body: string;
}
