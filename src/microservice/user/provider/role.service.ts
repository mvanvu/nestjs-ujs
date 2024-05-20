import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma, UserStatus } from '.prisma/user';
import { RoleEntity, CreateRoleDto, UpdateRoleDto } from '@lib/service/user';
import { BaseService, CRUDService } from '@service/lib';
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
      _count: { userRoles: { where: { some: { user: { status: { not: UserStatus.Trashed } } } } } },
   };

   roleCRUD(): CRUDService<PrismaService, CreateRoleDto, UpdateRoleDto, Prisma.RoleSelect> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'role',
         select: this.roleSelect,
         events: {
            onEntity: RoleEntity,
            onBeforeDelete(role: RoleEntity) {
               if (role.totalUsers) {
                  ThrowException(`Can't delete the role which has some users who assigned to it`);
               }
            },
         },
      });
   }
}
