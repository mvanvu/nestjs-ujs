import { BaseEntity, Property } from '@lib';

export class UserEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   username: string;

   @Property()
   email: string;
}
