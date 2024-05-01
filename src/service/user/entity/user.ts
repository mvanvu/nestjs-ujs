import { BaseEntity, IProperty, UserRole } from '@lib';
import { PaginationMeta } from '@lib/entity/pagination';
import { ObjectRecord } from '@mvanvu/ujs';

export class UserEntity extends BaseEntity {
   @IProperty()
   id: string;

   @IProperty()
   name?: string;

   @IProperty()
   username?: string;

   @IProperty()
   email: string;

   @IProperty()
   roles: UserRole[];

   constructor(entity?: ObjectRecord) {
      super(entity);

      if (Array.isArray(entity?.userRoles)) {
         this.roles = entity.userRoles.map(({ role }) => role);
      }
   }
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
