import { EmailSchema, RawSchema, StringSchema } from '@shared-library';

export class SendMailDto {
   @StringSchema({ optional: true, empty: false })
   from?: string;

   @EmailSchema({ isArray: 'unique' })
   to: string[];

   @StringSchema({ empty: false })
   subject: string;

   @RawSchema({ empty: false })
   body: string;
}

export class SendTestMailDto {
   @EmailSchema()
   email: string;

   @StringSchema({ empty: false, swagger: { example: 'Test send email' } })
   subject: string;

   @RawSchema({ empty: false, swagger: { example: 'Send test mail successfully, this email for testing purpose' } })
   body: string;
}
