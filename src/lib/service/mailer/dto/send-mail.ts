import { IPickType } from '@lib/common/entity/mapped-type';
import { IProperty } from '@lib/common/decorator/property';
export class SendMailDto {
   @IProperty({
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
      optional: true,
   })
   from?: string;

   @IProperty({
      transform: { fromType: 'array', toType: ['toArrayUnique'] },
      validate: [{ is: 'array' }, { is: 'empty', not: true }],
   })
   to: string[];

   @IProperty({
      transform: { fromType: 'string', toType: ['toStripTags', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   subject: string;

   @IProperty({
      transform: { fromType: 'string', toType: ['toSafeHtml', 'trim'] },
      validate: [{ is: 'string' }, { is: 'empty', not: true }],
   })
   body: string;
}

export class SendTestMailDto extends IPickType(SendMailDto, ['to', 'subject', 'body']) {}
