import { BaseEntity, IProperty } from '@lib';
import { PaginationMeta } from '@lib/entity/pagination';

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

export class AuthTokens {
   @IProperty()
   access: string;

   @IProperty()
   refresh: string;
}

export class AuthEntity extends BaseEntity {
   @IProperty()
   user: UserEntity;

   @IProperty()
   tokens: AuthTokens;
}

export class SwaggerPaginationUserEntity {
   @IProperty({ swagger: { isArray: true } })
   data: UserEntity;

   @IProperty()
   meta: PaginationMeta;
}
