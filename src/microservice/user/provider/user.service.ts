import { CreateUserDto, UserSignInDto, UserSignUpDto, UpdateUserDto, ResetPasswordDto } from '../dto';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { Prisma, UserStatus, VerifyCode } from '.prisma/user';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is, JWTError, Schema } from '@mvanvu/ujs';
import { serviceConfig } from '@metadata';
import { BaseService } from '@microservice/@library';
import {
   FieldsException,
   ThrowException,
   DataMetaResult,
   UserEntity,
   AuthTokenEntity,
   AuthEntity,
   BaseEntity,
} from '@shared-library';

@Injectable()
export class UserService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly userInclude: Prisma.UserInclude = {
      group: { select: { id: true, name: true, roles: true, groups: true } },
   };

   private async validateUserDto(
      tx: Partial<PrismaService>,
      dto: Partial<UserSignUpDto & CreateUserDto & UpdateUserDto>,
      id?: string,
   ) {
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

      const existsUser = await tx.user.findFirst({
         where: { OR, id: id ? { not: id } : undefined },
         select: { username: true, email: true },
      });

      if (dto.groupId) {
         const group = await tx.group.findUnique({
            where: { id: dto.groupId },
            select: { id: true },
         });

         if (!group) {
            fieldsError.add('groupId', FieldsException.NOT_FOULND);
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

   async signUp(data: UserSignUpDto): Promise<DataMetaResult<UserEntity>> {
      await this.validateUserDto(this.prisma, data);
      const { name, username, email, password } = data;
      const newUser = await this.prisma.user
         .create({
            data: {
               name,
               username,
               email,
               password: await argon2.hash(password),
               status: UserStatus.Pending,
               verifyCode: {},
            },
            select: { id: true },
         })
         .then(({ id }) =>
            this.prisma.user.update({
               data: { verifyCode: { activateAccount: `${id}:${Hash.uuid()}` } },
               where: { id },
            }),
         );

      return { data: BaseEntity.bindToClass(newUser, UserEntity), meta: { verifyCode: newUser.verifyCode } };
   }

   async generateTokens(userId: string): Promise<AuthTokenEntity> {
      const jwtConfig = serviceConfig.get('user.jwt');
      const secret = jwtConfig.secret;
      const jwt = Hash.jwt();
      const [access, refresh] = await Promise.all([
         jwt.sign({ id: userId }, { secret, exp: DateTime.now().addMinute(jwtConfig.accessExpiresInMinutes) }),
         jwt.sign({ id: userId }, { secret, exp: DateTime.now().addMinute(jwtConfig.refreshExpiresInMinutes) }),
      ]);

      return { access, refresh };
   }

   async signIn(data: UserSignInDto): Promise<AuthEntity> {
      const { username, password } = data;
      const fieldsException = new FieldsException();

      if (Is.empty(username)) {
         fieldsException.add('username', FieldsException.REQUIRED).validate();
      }

      if (Is.empty(password)) {
         fieldsException.add('password', FieldsException.REQUIRED).validate();
      }

      fieldsException.validate();
      const OR: Prisma.UserWhereInput['OR'] = [{ username: { equals: username, mode: 'insensitive' } }];

      if (Schema.email().check(username)) {
         OR.push({ email: { equals: username, mode: 'insensitive' } });
      }

      const user = await this.prisma.user.findFirst({ where: { OR }, include: this.userInclude });

      if (!user || user.status !== UserStatus.Active || !(await argon2.verify(user.password, password))) {
         ThrowException('Invalid credentials');
      }

      return { user: BaseEntity.bindToClass(user, UserEntity), tokens: await this.generateTokens(user.id) };
   }

   async verifyToken(token: string): Promise<UserEntity> {
      try {
         const { id } = await Hash.jwt().verify<{ id: string }>(token, {
            secret: serviceConfig.get('user.jwt.secret'),
         });
         const user = await this.prisma.user.findUnique({ where: { id }, include: this.userInclude });

         if (user?.status !== UserStatus.Active) {
            ThrowException('Invalid credentials');
         }

         return BaseEntity.bindToClass(user, UserEntity);
      } catch (e) {
         if (e instanceof JWTError) {
            ThrowException('The token is invalid or expired');
         }

         throw e;
      }
   }

   async verifyAccount(code: string): Promise<UserEntity> {
      const [id] = code.split(':');
      const user = await this.prisma.user.findUnique({ where: { id }, select: { verifyCode: true, status: true } });

      if (!user || user.verifyCode.activateAccount !== code) {
         ThrowException('Invalid verify account code');
      }

      if (user.status !== UserStatus.Pending) {
         ThrowException('The account has verified');
      }

      return BaseEntity.bindToClass(
         await this.prisma.user.update({
            where: { id },
            data: { status: UserStatus.Active, verifyCode: { ...user.verifyCode, activateAccount: null } },
            include: this.userInclude,
         }),
         UserEntity,
      );
   }

   async resetPassword(dto: ResetPasswordDto): Promise<UserEntity> {
      const [id] = dto.code.split(':');
      const user = await this.prisma.user.findUnique({ where: { id }, select: { verifyCode: true, status: true } });

      if (!user || user.verifyCode.resetPassword !== dto.code) {
         ThrowException('Invalid verify reset password code');
      }

      if (user.status !== UserStatus.Active) {
         ThrowException(`The account isn't available`);
      }

      return BaseEntity.bindToClass(
         await this.prisma.user.update({
            where: { id },
            data: {
               password: await argon2.hash(dto.password),
               verifyCode: { ...user.verifyCode, resetPassword: null },
            },
            include: this.userInclude,
         }),
         UserEntity,
      );
   }

   async refreshToken(token: string): Promise<AuthTokenEntity> {
      const user = await this.verifyToken(token);

      return await this.generateTokens(user.id);
   }

   async updateResetPasswordCode(email: string): Promise<false | DataMetaResult<UserEntity>> {
      const user = await this.prisma.user.findUnique({
         where: { email },
         select: { id: true, status: true, verifyCode: true },
      });

      if (user && user.status === UserStatus.Active) {
         const verifyCode: VerifyCode = user.verifyCode || <VerifyCode>{};
         verifyCode.resetPassword = `${user.id}:${Hash.uuid()}`;

         return {
            data: BaseEntity.bindToClass(
               await this.prisma.user.update({ where: { id: user.id }, data: { verifyCode } }),
               UserEntity,
            ),
            meta: { verifyCode },
         };
      }

      return false;
   }

   createCRUDService() {
      return this.prisma
         .createCRUDService('user', { entity: UserEntity, createDto: CreateUserDto, updateDto: UpdateUserDto })
         .config({
            // softDelete: !appConfig.is('nodeEnv', 'test'),
            list: { searchFields: ['name', 'username', 'email'], filterFields: ['username', 'email'] },
         })
         .include(this.userInclude)
         .beforeCreate(async ({ tx, data }) => {
            await this.validateUserDto(tx, data);

            if (data.password) {
               data.password = await argon2.hash(data.password);
            }
         })
         .beforeUpdate(async ({ tx, data, record }) => {
            if (data.email || data.username || data.groupId) {
               await this.validateUserDto(tx, data, record.id);
            }

            if (data.password) {
               data.password = await argon2.hash(data.password);
            }
         });
   }
}
