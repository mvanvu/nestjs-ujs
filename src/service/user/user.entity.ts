import { BaseEntity, IProperty } from '@lib';

export class UserEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name?: string;

   @IProperty()
   username?: string;

   @IProperty()
   email: string;
}
