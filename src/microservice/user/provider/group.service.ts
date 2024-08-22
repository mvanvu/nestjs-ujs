import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CreateGroupDto, UpdateGroupDto } from '../dto';
import { BaseService } from '@microservice/@library';
import { ThrowException, FieldsException, GroupEntity } from '@shared-library';
import { Prisma, AvailableStatus } from '.prisma/user';
import { Is } from '@mvanvu/ujs';

@Injectable()
export class GroupService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   private async validateData(tx: Partial<PrismaService>, data: UpdateGroupDto): Promise<void> {
      const fieldsException = new FieldsException();

      if (data.groups?.length) {
         const groups = [];

         await Promise.all(
            data.groups.map((groupId, index) =>
               (async () => {
                  const group = await tx.group.findUnique({
                     where: { id: groupId },
                     select: { id: true, name: true, roles: true },
                  });

                  if (!group) {
                     fieldsException.add(
                        `groups[${index}]`,
                        this.language._('USER_GROUP_$ID_NOT_EXISTS', { id: groupId }),
                        FieldsException.NOT_FOULND,
                     );
                  }

                  groups.push(group);
               })(),
            ),
         );

         data.groups = groups;
      }

      if (data.roles?.length) {
         const roles = [];
         await Promise.all(
            data.roles.map((roleId, index) =>
               (async () => {
                  const role = await tx.role.findUnique({
                     where: { id: roleId },
                     select: { id: true, name: true, permissions: true },
                  });

                  if (!role) {
                     fieldsException.add(
                        `roles[${index}]`,
                        this.language._('USER_ROLE_$ID_NOT_EXISTS', { id: roleId }),
                        FieldsException.NOT_FOULND,
                     );
                  }

                  roles.push(role);
               })(),
            ),
         );

         data.roles = roles;
      }

      fieldsException.validate();
   }

   createCRUDService() {
      return this.prisma
         .createCRUDService('group', { entity: GroupEntity, createDto: CreateGroupDto, updateDto: UpdateGroupDto })
         .include(<Prisma.GroupInclude>{ _count: { select: { users: { where: { status: AvailableStatus.Active } } } } })
         .boot(({ context, data }) => {
            if (context === 'create') {
               const dto = data as CreateGroupDto;

               if (dto.groups === undefined) {
                  dto.groups = [];
               }

               if (dto.roles === undefined) {
                  dto.roles = [];
               }
            }
         })
         .beforeCreate(async ({ tx, data }) => {
            await this.validateData(tx, data);
         })
         .beforeUpdate(async ({ tx, data }) => {
            await this.validateData(tx, data);
         })
         .afterUpdate(async ({ record, tx }) => {
            const groups = await tx.group.findMany({
               where: { groups: { some: { id: record.id } } },
               select: { id: true, groups: true },
            });

            if (groups.length) {
               await Promise.all(
                  groups.map((group) => {
                     const index = group.groups.findIndex(({ id }) => id === record.id);

                     if (index !== -1) {
                        group.groups[index] = { id: record.id, name: record.name, roles: record.roles };
                        return tx.group.update({ data: { groups: group.groups }, where: { id: group.id } });
                     }
                  }),
               );
            }
         })
         .beforeDelete(({ record }) => {
            if (record.totalActiveUsers) {
               ThrowException(this.language._('USER_$GROUP_HAS_USERS_CANT_DELETE', { group: record.name }));
            }
         })
         .afterDelete(async ({ record, tx }) => {
            const groups = await tx.group.findMany({
               where: { groups: { some: { id: record.id } } },
               select: { id: true, groups: true },
            });

            if (groups.length) {
               await Promise.all(
                  groups.map((group) => {
                     const index = group.groups.findIndex(({ id }) => id === record.id);

                     if (index !== -1) {
                        group.groups.splice(index, 1);
                        return tx.group.update({ data: { groups: group.groups }, where: { id: group.id } });
                     }
                  }),
               );
            }
         });
   }
}
