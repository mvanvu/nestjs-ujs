import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CreateRoleDto, UpdateRoleDto } from '../dto';
import { BaseService, CRUDService } from '@microservice/@library';
import { CRUDExecuteContext, RoleEntity, ThrowException } from '@shared-library';

@Injectable()
export class RoleService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Role')
         .validateDTOPipe(CreateRoleDto, UpdateRoleDto)
         .entityResponse(RoleEntity)
         .beforeExecute<RoleEntity, UpdateRoleDto, CRUDExecuteContext>(async ({ data, record, context }) => {
            if (!data.permissions && context === 'create') {
               data.permissions = [];
            }

            if (context === 'delete') {
               const hasSomeUsers = await this.prisma.group.count({ where: { roles: { some: { id: record.id } } } });

               if (hasSomeUsers) {
                  ThrowException(`The role name(${record.name}) has been assigned to some groups, can't delete`);
               }
            }
         });
   }
}
