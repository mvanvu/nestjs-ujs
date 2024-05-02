import { BaseService, ServiceOptions, CRUDService, CreateCRUDService } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { userConfig } from '../user.config';
import { RoleEntity } from '@service/user/entity';

@Injectable()
export class RoleService extends BaseService implements CreateCRUDService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly options: ServiceOptions = { config: userConfig };

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
