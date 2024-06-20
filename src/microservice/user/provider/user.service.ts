import { CreateUserDto, UserSignInDto, UserSignUpDto, UpdateUserDto, ResetPasswordDto } from '../dto';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { Prisma, UserStatus, VerifyCode } from '.prisma/user';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is, JWTError } from '@mvanvu/ujs';
import { serviceConfig } from '@metadata';
import { BaseService, CRUDService } from '@microservice/@library';
import {
   FieldsException,
   ThrowException,
   MetaResult,
   CRUDExecuteContext,
   UserEntity,
   AuthTokenEntity,
   AuthEntity,
} from '@shared-library';

@Injectable()
export class UserService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly userInclude: Prisma.UserInclude = {
      group: { select: { id: true, name: true, roles: true, groups: true } },
   };

   private async validateUserDto(dto: Partial<UserSignUpDto & CreateUserDto & UpdateUserDto>, id?: string) {
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

      if (dto.groupId) {
         const group = await this.prisma.group.findUnique({
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

   async signUp(data: UserSignUpDto): Promise<MetaResult<UserEntity>> {
      await this.validateUserDto(data);
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

      return { data: new UserEntity(newUser), meta: { verifyCode: newUser.verifyCode } };
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

      if (Is.empty(username)) {
         ThrowException('Invalid credentials');
      }

      const OR: Prisma.UserWhereInput['OR'] = [{ username: { equals: username, mode: 'insensitive' } }];

      if (Is.email(username)) {
         OR.push({ email: { equals: username, mode: 'insensitive' } });
      }

      const user = await this.prisma.user.findFirst({ where: { OR }, include: this.userInclude });

      if (!user || user.status !== UserStatus.Active || !(await argon2.verify(user.password, password))) {
         ThrowException('Invalid credentials');
      }

      return new AuthEntity({ user: new UserEntity(user), tokens: await this.generateTokens(user.id) });
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

         return new UserEntity(user);
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

      return new UserEntity(
         await this.prisma.user.update({
            where: { id },
            data: { status: UserStatus.Active, verifyCode: { ...user.verifyCode, activateAccount: null } },
            include: this.userInclude,
         }),
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

      return new UserEntity(
         await this.prisma.user.update({
            where: { id },
            data: {
               password: await argon2.hash(dto.password),
               verifyCode: { ...user.verifyCode, resetPassword: null },
            },
            include: this.userInclude,
         }),
      );
   }

   async refreshToken(token: string): Promise<AuthTokenEntity> {
      const user = await this.verifyToken(token);

      return await this.generateTokens(user.id);
   }

   async updateResetPasswordCode(email: string): Promise<false | MetaResult<UserEntity>> {
      const user = await this.prisma.user.findUnique({
         where: { email },
         select: { id: true, status: true, verifyCode: true },
      });

      if (user && user.status === UserStatus.Active) {
         const verifyCode: VerifyCode = user.verifyCode || <VerifyCode>{};
         verifyCode.resetPassword = `${user.id}:${Hash.uuid()}`;

         return {
            data: new UserEntity(await this.prisma.user.update({ where: { id: user.id }, data: { verifyCode } })),
            meta: { verifyCode },
         };
      }

      return false;
   }

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('User')
         .options({
            softDelete: true,
            list: { searchFields: ['name', 'username', 'email'], filterFields: ['username', 'email'] },
         })
         .entityResponse(UserEntity)
         .include(this.userInclude)
         .validateDTOPipe(CreateUserDto, UpdateUserDto)
         .beforeExecute<UserEntity, UpdateUserDto, CRUDExecuteContext>(async ({ data, record: user, context }) => {
            if (context === 'create') {
               await this.validateUserDto(data);
            } else if (context === 'update') {
               if (data.email || data.username || data.groupId) {
                  await this.validateUserDto(data, user.id);
               }

               // Verify permission
               const author = this.user;
               const isSelf = author.id === user.id;

               if (isSelf && data.status) {
                  ThrowException(`You can't update yourself status`);
               }

               if (data.groupId && isSelf && !author.isRoot) {
                  ThrowException(`You can't update your group because you aren't a root user`);
               }

               // Check the current user is the author or editor of the target user, then will can update
               const isGranter = user.createdBy === author.id || user.updatedBy === author.id;

               if (!isGranter && !(isSelf && author.isRoot)) {
                  const compare = author.compare(user, serviceConfig.get('user.permissions.user.update'));

                  if (compare === 0) {
                     ThrowException(`You can't update the user who has the same permission with you`);
                  }

                  if (compare === -1) {
                     ThrowException(`You can't update the user who has the greater permissions than you`);
                  }
               }
            } else if (context === 'delete') {
               // Verify permission
               const author = new UserEntity(this.meta.get('headers.user'));
               const isSelf = author.id === user.id;

               if (isSelf) {
                  return;
                  // ThrowException(`You can't delete yourself`);
               }

               // Check the current user is the author or editor of the target user, then will can delete
               const isGranter = user.createdBy === author.id || user.updatedBy === author.id;

               if (!isGranter) {
                  const compare = author.compare(user, serviceConfig.get('user.permissions.user.delete'));

                  if (compare === 0) {
                     ThrowException(`You can't delete the user who has the same permission with you`);
                  }

                  if (compare === -1) {
                     ThrowException(`You can't delete the user who has the greater permissions than you`);
                  }
               }
            }

            if (data.password) {
               data.password = await argon2.hash(data.password);
            }
         });
   }
}
