import { Property } from '@lib/decorator/property';
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
