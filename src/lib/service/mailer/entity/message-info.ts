import { IProperty } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity extends BaseEntity {
   @IProperty()
   messageId: string;

   @IProperty({ swagger: { type: [String] } })
   accepted: Array<string | Mail.Address>;

   @IProperty({ swagger: { type: [String] } })
   rejected: Array<string | Mail.Address>;

   @IProperty({ swagger: { type: [String] } })
   pending: Array<string | Mail.Address>;

   @IProperty()
   response: string;
}
