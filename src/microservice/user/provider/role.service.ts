import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CreateRoleDto, UpdateRoleDto } from '../dto';
import { BaseService } from '@microservice/@library';
import { RoleEntity, ThrowException } from '@shared-library';

@Injectable()
export class RoleService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService() {
      return this.prisma
         .createCRUDService('role', { entity: RoleEntity, createDto: CreateRoleDto, updateDto: UpdateRoleDto })
         .beforeCreate(({ data }) => {
            if (!data.permissions) {
               data.permissions = [];
            }
         })
         .beforeDelete(async ({ tx, record }) => {
            const hasSomeUsers = await tx.group.count({ where: { roles: { some: { id: record.id } } } });

            if (hasSomeUsers) {
               ThrowException(this.language._('USER_$ROLE_HAS_USERS_CANT_DELETE', { name: record.name }));
            }
         });
   }
}
