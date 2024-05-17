import { CreateUserDto, UserSignInDto, UserSignUpDto, AuthEntity, UserEntity, UpdateUserDto } from '@lib/service';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma, UserStatus } from '.prisma/user';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is, Registry } from '@mvanvu/ujs';
import { appConfig } from '@config';
import { BaseService, CRUDService, CreateCRUDService } from '@service/lib';
import { FieldsException, ThrowException } from '@lib/common';

@Injectable()
export class UserService extends BaseService implements CreateCRUDService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly userSelect: Prisma.UserSelect = {
      id: true,
      username: true,
      name: true,
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
                  permissions: true,
               },
            },
         },
      },
   };

   private async validateUserDto(dto: UserSignUpDto | CreateUserDto | UpdateUserDto, id?: string) {
      const fieldsError = new FieldsException();
      let { username, email } = dto;
      const OR: Prisma.UserWhereInput['OR'] = [];

      if (username) {
         username = username.toLowerCase();
         OR.push({ username });
      }

      if (email) {
         email = email.toLowerCase();
         OR.push({ email });
      }

      const existsUser = await this.prisma.user.findFirst({
         where: { OR, id: id ? { not: id } : undefined },
         select: { username: true, email: true },
      });

      if (dto['roles']?.length) {
         let isValidRoles = false;

         try {
            const count = await this.prisma.role.count({ where: { id: { in: dto['roles'] } } });
            isValidRoles = count === dto['roles'].length;
         } catch {}

         if (!isValidRoles) {
            fieldsError.add('roles', FieldsException.NOT_FOULND);
         }
      }

      if (existsUser) {
         if (username && existsUser.username === username) {
            fieldsError.add('username', FieldsException.ALREADY_EXISTS);
         }

         if (existsUser.email === email) {
            fieldsError.add('email', FieldsException.ALREADY_EXISTS);
         }
      }

      fieldsError.validate();
   }

   async signUp(data: UserSignUpDto): Promise<UserEntity> {
      await this.validateUserDto(data);
      const { name, username, email, password } = data;
      const newUser = await this.prisma.user.create({
         data: { name, username, email, password: await argon2.hash(password) },
      });

      return new UserEntity(newUser);
   }

   async generateTokens(userId: string): Promise<AuthEntity['tokens']> {
      const secret = appConfig.get<string>('jwt.secret');
      const jwt = Hash.jwt();
      const [access, refresh] = await Promise.all([
         jwt.sign(
            { id: userId },
            { secret, exp: DateTime.now().addMinute(appConfig.get<number>('jwt.accessExpiresInMinutes')) },
         ),
         jwt.sign(
            { id: userId },
            { secret, exp: DateTime.now().addMinute(appConfig.get<number>('jwt.refreshExpiresInMinutes')) },
         ),
      ]);

      return { access, refresh };
   }

   async signIn(data: UserSignInDto): Promise<AuthEntity> {
      const { username, password } = data;

      if (Is.empty(username)) {
         ThrowException('Invalid credentials');
      }

      const OR: Prisma.UserWhereInput['OR'] = [{ username: { equals: username, mode: 'insensitive' } }];

      if (Is.email(username)) {
         OR.push({ email: { equals: username, mode: 'insensitive' } });
      }

      const user = await this.prisma.user.findFirst({
         where: { OR },
         select: this.userSelect,
      });

      if (!user || user.status !== UserStatus.Active || !(await argon2.verify(user.password, password))) {
         ThrowException('Invalid credentials');
      }

      return new AuthEntity({ user: new UserEntity(user), tokens: await this.generateTokens(user.id) });
   }

   async verify(token: string): Promise<UserEntity> {
      const { id } = await Hash.jwt().verify<{ id: string }>(token, { secret: appConfig.get('jwt.secret') });
      const user = await this.prisma.user.findUnique({ where: { id }, select: this.userSelect });

      if (!user || user.status !== UserStatus.Active) {
         ThrowException('Invalid credentials');
      }

      return new UserEntity(user);
   }

   createCRUDService(): CRUDService<{
      TPrismaService: PrismaService;
      TCreateDTO: CreateUserDto;
      TUpdateDTO: UpdateUserDto;
      TPrismaSelect: Prisma.UserSelect;
   }> {
      return new CRUDService({
         prisma: this.prisma,
         model: 'user',
         select: Registry.from(this.userSelect).remove('userRoles.select.role.select.permissions').valueOf(),
         events: {
            onEntity: UserEntity,
            onBeforeCreate: async (data: CreateUserDto) => {
               await this.validateUserDto(data);
               Object.assign(data, {
                  userRoles: data.roles?.length ? { create: data.roles.map((roleId) => ({ roleId })) } : undefined,
                  password: await argon2.hash(data.password),
               });
            },
            onBeforeUpdate: async (data: UpdateUserDto, record: UserEntity) => {
               if (data.email || data.username || data.roles) {
                  await this.validateUserDto(data, record.id);
               }

               if (data.roles) {
                  data['userRoles'] = {
                     deleteMany: {},
                     create: data.roles.length ? data.roles.map((roleId) => ({ roleId })) : undefined,
                  };
               }

               if (data.password) {
                  data.password = await argon2.hash(data.password);
               }
            },
         },
      });
   }
}
