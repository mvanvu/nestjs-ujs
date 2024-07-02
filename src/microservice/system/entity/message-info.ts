import { BaseEntity, StringSchema } from '@shared-library';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity extends BaseEntity {
   @StringSchema()
   messageId: string;

   @StringSchema({ each: 'unique' })
   accepted: Array<string | Mail.Address>;

   @StringSchema({ each: 'unique' })
   rejected: Array<string | Mail.Address>;

   @StringSchema({ each: 'unique' })
   pending: Array<string | Mail.Address>;

   @StringSchema()
   response: string;
}
