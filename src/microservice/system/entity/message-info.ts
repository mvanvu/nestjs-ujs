import { StringSchema } from '@shared-library';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity {
   @StringSchema({ empty: false })
   messageId: string;

   @StringSchema({ isArray: 'unique' })
   accepted: Array<string | Mail.Address>;

   @StringSchema({ isArray: 'unique' })
   rejected: Array<string | Mail.Address>;

   @StringSchema({ isArray: 'unique' })
   pending: Array<string | Mail.Address>;

   @StringSchema({ empty: false })
   response: string;
}
