import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateGroupDto, GroupEntity } from '@lib/service/user';
import { BaseService } from '@service/lib';
import { CRUDResult, ThrowException } from '@lib/common';

@Injectable()
export class GroupService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   executeCRUD(): Promise<CRUDResult<GroupEntity>> {
      return this.prisma
         .createCRUDService('Group')
         .validateDTOPipe(CreateGroupDto)
         .entityResponse(GroupEntity)
         .transaction(async (tx: PrismaService, options: { record: GroupEntity; data: Partial<CreateGroupDto> }) => {
            const { record, data } = options;
            const groupsData = [];
            const rolesData = [];

            if (data.groupIds.length) {
               await Promise.all(
                  data.groupIds.map((groupId) => async () => {
                     const group = await tx.group.findUnique({
                        where: { id: groupId },
                        select: { id: true, name: true, roles: true },
                     });

                     if (!group) {
                        ThrowException(`The group ID(${groupId}) doesn't exists`);
                     }

                     groupsData.push(group);
                  }),
               );
            }

            if (data.roleIds.length) {
               await Promise.all(
                  data.roleIds.map((roleId) => async () => {
                     const role = await tx.role.findUnique({
                        where: { id: roleId },
                        select: { id: true, name: true, permissions: true },
                     });

                     if (!role) {
                        ThrowException(`The role ID(${roleId}) doesn't exists`);
                     }

                     rolesData.push(role);
                  }),
               );
            }

            if (groupsData.length || rolesData.length) {
               await tx.group.update({
                  where: { id: record.id },
                  data: {
                     groups: groupsData.length ? groupsData : undefined,
                     roles: rolesData.length ? rolesData : undefined,
                  },
               });
            }
         })
         .execute();
   }
}
