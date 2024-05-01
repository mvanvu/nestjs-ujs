import { BaseService, ServiceOptions, CRUDService, CreateCRUDService, MessageData, FieldsException } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { userConfig } from '../user.config';
import { RoleEntity } from '@service/user/entity';
import { PermissionDto, PermissionListDto } from '@service/user/dto';
import { Registry, Transform, Util } from '@mvanvu/ujs';

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
      permissions: {
         select: { refModel: true, canCreate: true, canRead: true, canUpdate: true, canDelete: true },
      },
   };

   createCRUDService(): CRUDService<PrismaService, Prisma.RoleSelect, any, any> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'role',
         select: this.roleSelect,
         events: { onEntity: RoleEntity },
      });
   }

   async createPermissions({
      data,
      meta,
   }: MessageData<
      PermissionListDto,
      Registry<{ params: { roleId: string }; headers: { user: { id: string } } }>
   >): Promise<RoleEntity> {
      const roleId = meta.get<string>('params.roleId');
      const hasExists = !!(await this.prisma.role.findUnique({ where: { id: roleId }, select: { id: true } }));

      if (!hasExists) {
         new FieldsException().add('roleId', FieldsException.NOT_FOULND).validate();
      }

      const createManyData: Array<{ roleId: string } & PermissionDto> = [];
      const refModels: string[] = [];

      for (const permission of data.permissions) {
         permission.refModel = Util.lFirst(permission.refModel);

         if (!this.prisma[permission.refModel]) {
            new FieldsException().add(`refModel[${permission.refModel}]`, FieldsException.NOT_FOULND).validate();
         }

         if (!refModels.includes(permission.refModel)) {
            refModels.push(permission.refModel);
         }

         createManyData.push({ roleId, ...permission });
      }

      return await this.prisma.permission
         .deleteMany({ where: { roleId, refModel: { in: refModels } } })
         .then(() =>
            this.prisma.permission
               .createMany({ data: Transform.toArrayUnique(createManyData) })
               .then(() =>
                  this.createCRUDService().update<RoleEntity>(roleId, { updatedBy: meta.get('headers.user.id') }),
               ),
         );
   }
}
