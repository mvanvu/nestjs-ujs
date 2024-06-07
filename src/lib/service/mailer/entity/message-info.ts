import { Property } from '@lib/common/decorator/property';
import { BaseEntity } from '@lib/common/entity/base';
import Mail from 'nodemailer/lib/mailer';

export class MessageInfoEntity extends BaseEntity {
   @Property()
   messageId: string;

   @Property({ swagger: { type: [String] } })
   accepted: Array<string | Mail.Address>;

   @Property({ swagger: { type: [String] } })
   rejected: Array<string | Mail.Address>;

   @Property({ swagger: { type: [String] } })
   pending: Array<string | Mail.Address>;

   @Property()
   response: string;
}
