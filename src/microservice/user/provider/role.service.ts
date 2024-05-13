import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { RoleEntity } from '@lib/service/user/entity';
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
      permissions: true,
   };

   createCRUDService(): CRUDService<PrismaService, Prisma.RoleSelect, any, any> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'role',
         select: this.roleSelect,
         events: { onEntity: RoleEntity },
      });
   }
}
