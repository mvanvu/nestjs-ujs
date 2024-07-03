import { StringSchema } from '@shared-library';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity {
   @StringSchema()
   messageId: string;

   @StringSchema({ isArray: 'unique' })
   accepted: Array<string | Mail.Address>;

   @StringSchema({ isArray: 'unique' })
   rejected: Array<string | Mail.Address>;

   @StringSchema({ isArray: 'unique' })
   pending: Array<string | Mail.Address>;

   @StringSchema()
   response: string;
}
