import { IPickType } from '@lib/common/entity/mapped-type';
import { Property } from '@lib/common/decorator/property';
export class SendMailDto {
   @Property({
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      optional: true,
   })
   from?: string;

   @Property({
      transform: { fromType: 'array', toType: ['toArrayUnique'] },
      validate: [{ is: 'array' }, { is: 'empty', not: true }],
   })
   to: string[];

   @Property({
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   subject: string;

   @Property({
      transform: { fromType: 'string', toType: ['toSafeHtml', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   body: string;
}

export class SendTestMailDto extends IPickType(SendMailDto, ['to', 'subject', 'body']) {}
