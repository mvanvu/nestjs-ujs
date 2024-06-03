import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateRoleDto, RoleEntity, UpdateRoleDto } from '@lib/service/user';
import { BaseService } from '@service/lib';
import { CRUDResult } from '@lib/common';

@Injectable()
export class RoleService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   executeCRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.prisma
         .createCRUDService('Role')
         .validateDTOPipe(CreateRoleDto, UpdateRoleDto)
         .entityResponse(RoleEntity)
         .beforeSave((dto: CreateRoleDto | UpdateRoleDto, { context }) => {
            if (!dto.permissions && context === 'create') {
               dto.permissions = [];
            }
         })
         .execute();
   }
}
