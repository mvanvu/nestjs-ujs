import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { RoleEntity, CreateRoleDto, UpdateRoleDto } from '@lib/service/user';
import { BaseService, CRUDService, CreateCRUDService } from '@service/lib';

@Injectable()
export class RoleService extends BaseService implements CreateCRUDService {
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
   };

   createCRUDService(): CRUDService<{
      TPrismaService: PrismaService;
      TCreateDTO: CreateRoleDto;
      TUpdateDTO: UpdateRoleDto;
      TPrismaSelect: Prisma.RoleSelect;
   }> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'role',
         select: this.roleSelect,
         events: { onEntity: RoleEntity },
      });
   }
}
