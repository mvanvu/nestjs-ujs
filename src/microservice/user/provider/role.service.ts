import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma, UserStatus } from '.prisma/user';
import { RoleEntity } from '@lib/service/user';
import { BaseService } from '@service/lib';
import { ThrowException } from '@lib/common';

@Injectable()
export class RoleService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly roleSelect: Prisma.RoleSelect = {
      id: true,
      name: true,
      description: true,
      root: true,
      createdAt: true,
      updatedAt: true,
      author: { select: { id: true, email: true, username: true } },
      editor: { select: { id: true, email: true, username: true } },
      _count: {
         select: {
            userRoles: { where: { user: { status: { not: UserStatus.Trashed } } } },
         },
      },
   };

   roleCRUD() {
      return this.prisma
         .createCRUD('role')
         .select(this.roleSelect)
         .entity(RoleEntity)
         .beforeDelete((role: RoleEntity) => {
            if (role.totalUsers) {
               ThrowException(`Can't delete the role which has some users who assigned to it`);
            }
         });
   }
}
