import {
   BaseService,
   ServiceOptions,
   CRUDService,
   FieldsException,
   MessageData,
   ThrowException,
   appConfig,
   CreateCRUDService,
} from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { AuthEntity, UserEntity } from '../entity/user';
import { userConfig } from '../user.config';
import { CreateUserDto, UserSignInDto, UserSignUpDto } from '../dto';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is } from '@mvanvu/ujs';

@Injectable()
export class UserService extends BaseService implements CreateCRUDService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly options: ServiceOptions = { config: userConfig };

   readonly userSelect: Prisma.UserSelect = {
      id: true,
      username: true,
      email: true,
      createdAt: true,
      updatedAt: true,
      status: true,
      userRoles: {
         select: {
            role: {
               select: {
                  id: true,
                  name: true,
                  root: true,
                  permissions: true,
               },
            },
         },
      },
   };

   createCRUDService(): CRUDService<PrismaService, Prisma.UserSelect, any, any> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'user',
         select: this.userSelect,
         events: {
            onEntity: UserEntity,
         },
      });
   }

   async hashPassword(rawPassword: string): Promise<string> {
      return await argon2.hash(rawPassword);
   }

   private async validateUserDto(dto: UserSignUpDto | CreateUserDto) {
      const { username, email } = dto;
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
   }

   async signUp({ data }: MessageData<UserSignUpDto>): Promise<UserEntity> {
      await this.validateUserDto(data);
      const { name, username, email, password } = data;
      const newUser = await this.prisma.user.create({
         data: { name, username, email, password: await this.hashPassword(password) },
      });

      return new UserEntity(newUser);
   }

   async generateTokens(userId: string): Promise<AuthEntity['tokens']> {
      const { jwt: jwtConfig } = appConfig;
      const jwt = Hash.jwt();
      const [access, refresh] = await Promise.all([
         jwt.sign(
            { id: userId },
            { secret: jwtConfig.secret, iat: DateTime.now().addMinute(jwtConfig.accessExpiresInMinutes) },
         ),

         jwt.sign(
            { id: userId },
            { secret: jwtConfig.secret, iat: DateTime.now().addMinute(jwtConfig.refreshExpiresInMinutes) },
         ),
      ]);

      return { access, refresh };
   }

   async signIn({ data }: MessageData<UserSignInDto>): Promise<AuthEntity> {
      const { username, email, password } = data;

      if (Is.empty([username, email], true)) {
         ThrowException('Invalid credentials');
      }

      const user = await this.prisma.user.findFirst({ where: { OR: [{ username }, { email }] } });

      if (!user || !(await argon2.verify(user.password, password))) {
         ThrowException('Invalid credentials');
      }

      return new AuthEntity({ user: new UserEntity(user), tokens: await this.generateTokens(user.id) });
   }

   async verify({ data: token }: MessageData<string>): Promise<UserEntity> {
      const { id } = await Hash.jwt().verify<{ id: string }>(token, { secret: appConfig.jwt.secret });
      const user = await this.prisma.user.findUnique({ where: { id }, select: this.userSelect });

      if (!user || user.status !== this.prisma.enums.UserStatus.ACTIVE) {
         ThrowException('Invalid credentials');
      }

      return new UserEntity(user);
   }

   async create({ data }: MessageData<CreateUserDto>): Promise<UserEntity> {
      await this.validateUserDto(data);
      const { name, username, email, password, roles } = data;
      const newUser = await this.prisma.user.create({
         data: {
            name,
            username,
            email,
            password: await this.hashPassword(password),
            userRoles: roles ? { create: roles.map((roleId) => ({ roleId })) } : undefined,
         },
         select: this.userSelect,
      });

      return new UserEntity(newUser);
   }
}
