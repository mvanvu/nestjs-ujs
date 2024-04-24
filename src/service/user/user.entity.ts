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

export class AuthEntity extends BaseEntity {
   @IProperty()
   user: UserEntity;

   @IProperty()
   tokens: { access: string; refresh: string };
}
