import { Property } from '../decorator/property';
import { BaseEntity } from './base';

export class UserRefEntity extends BaseEntity {
   @Property()
   id: string;

   @Property()
   name?: string;

   @Property()
   username?: string;

   @Property()
   email?: string;

   @Property()
   avatarUrl?: string;
}
