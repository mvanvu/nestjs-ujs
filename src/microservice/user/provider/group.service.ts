import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateGroupDto, GroupEntity, UpdateGroupDto } from '@lib/service/user';
import { BaseService } from '@service/lib';
import { CRUDResult, ThrowException } from '@lib/common';

@Injectable()
export class GroupService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   executeCRUD(): Promise<CRUDResult<GroupEntity>> {
      return this.prisma
         .createCRUDService('Group')
         .validateDTOPipe(CreateGroupDto, UpdateGroupDto)
         .entityResponse(GroupEntity)
         .beforeSave(async (data: Partial<CreateGroupDto>, { context }) => {
            const roles = [];

            if (data.groupIds?.length) {
               const groups = [];

               await Promise.all(
                  data.groupIds.map((groupId) => async () => {
                     const group = await this.prisma.group.findUnique({
                        where: { id: groupId },
                        select: { id: true, name: true, roles: true },
                     });

                     if (!group) {
                        ThrowException(`The group ID(${groupId}) doesn't exists`);
                     }

                     groups.push(group);
                  }),
               );

               data['groups'] = groups;
            }

            if (data.roleIds?.length) {
               await Promise.all(
                  data.roleIds.map((roleId) => async () => {
                     const role = await this.prisma.role.findUnique({
                        where: { id: roleId },
                        select: { id: true, name: true, permissions: true },
                     });

                     if (!role) {
                        ThrowException(`The role ID(${roleId}) doesn't exists`);
                     }

                     roles.push(role);
                  }),
               );

               data['roles'] = roles;
            }

            if (context === 'create') {
               if (!data['groups']) {
                  data['groups'] = [];
               }

               if (!data['roles']) {
                  data['roles'] = [];
               }
            }
         })
         .transaction<Partial<CreateGroupDto>, GroupEntity>(async (tx: PrismaService, { data, record, context }) => {
            if (context === 'create' || (!data.groupIds?.length && !data.roleIds?.length)) {
               return;
            }

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
         .execute();
   }
}
