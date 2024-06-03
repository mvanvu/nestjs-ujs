import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateRoleDto, RoleEntity } from '@lib/service/user';
import { BaseService } from '@service/lib';
import { CRUDResult } from '@lib/common';

@Injectable()
export class RoleService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   executeCRUD(): Promise<CRUDResult<RoleEntity>> {
      return this.prisma
         .createCRUDService('Role')
         .validateDTOPipe(CreateRoleDto)
         .entityResponse(RoleEntity)
         .beforeSave((dto: CreateRoleDto) => {
            if (!dto.permissions) {
               dto.permissions = [];
            }
         })
         .execute();
   }
}
