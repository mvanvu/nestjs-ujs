import { BaseService, ServiceOptions, CRUDService, PaginationResult, FieldsException, MessageData } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { UserEntity } from './user.entity';
import { userConfig } from './user.config';
import { UserSignUpDto } from './dto';
import * as argon2 from 'argon2';

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

   async hashPassword(rawPassword: string): Promise<string> {
      return await argon2.hash(rawPassword);
   }

   async signUp({ data }: MessageData<UserSignUpDto>): Promise<UserEntity> {
      const { username, name, password, email } = data;
      const existsUser = await this.prisma.user.findFirst({
         where: { OR: [{ username }, { email }] },
         select: { username: true, email: true },
      });

      if (existsUser) {
         const fieldsError = new FieldsException();

         if (username && existsUser.username === username.toLowerCase()) {
            fieldsError.add('username', FieldsException.ALREADY_EXISTS);
         }

         if (existsUser.email === email.toLowerCase()) {
            fieldsError.add('email', FieldsException.ALREADY_EXISTS);
         }

         fieldsError.validate();
      }

      const newUser = await this.prisma.user.create({
         data: { name, username, email, password: await this.hashPassword(password) },
      });

      return new UserEntity(newUser);
   }

   async paginate({ meta }: MessageData): Promise<PaginationResult<UserEntity>> {
      return this.createCRUDService().paginate(meta.get('query'));
   }
}
