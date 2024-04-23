import { BaseService, ServiceOptions, CRUDService, PaginationResult } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { UserEntity } from './user.entity';
import { userConfig } from './user.config';

@Injectable()
export class UserService extends BaseService {
   readonly options: ServiceOptions = { config: userConfig };

   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService, Prisma.UserSelect> {
      return new CRUDService<PrismaService, Prisma.UserSelect>({
         prisma: this.prisma,
         modelName: 'user',
         entity: UserEntity,
         select: {
            id: true,
            username: true,
            email: true,
         },
      });
   }

   async signUp(data: any) {
      console.log(data, this);

      return data;
   }

   async paginate(): Promise<PaginationResult<UserEntity>> {
      return this.createCRUDService().paginate();
   }
}
