import { CreateUserDto, UserSignInDto, UserSignUpDto, AuthEntity, UserEntity, UpdateUserDto } from '@lib/service';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma, UserStatus } from '.prisma/user';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is, JWTError } from '@mvanvu/ujs';
import { appConfig, serviceConfig } from '@metadata';
import { BaseService } from '@service/lib';
import { FieldsException, ThrowException, CRUDResult } from '@lib/common';

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

   async signUp(data: UserSignUpDto): Promise<UserEntity> {
      await this.validateUserDto(data);
      const { name, username, email, password } = data;
      const newUser = await this.prisma.user.create({
         data: { name, username, email, password: await argon2.hash(password) },
      });

      return new UserEntity(newUser);
   }

   async generateTokens(userId: string): Promise<AuthEntity['tokens']> {
      const jwtConfig = appConfig.get('jwt');
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
         const { id } = await Hash.jwt().verify<{ id: string }>(token, { secret: appConfig.get('jwt') });
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

   async refreshToken(token: string): Promise<AuthEntity> {
      const user = await this.verifyToken(token);

      return new AuthEntity({ user, tokens: await this.generateTokens(user.id) });
   }

   executeCRUD(): Promise<CRUDResult<UserEntity>> {
      return this.prisma
         .createCRUDService('User')
         .options({ softDelete: true, list: { searchFields: ['name', 'username', 'email'] } })
         .entityResponse(UserEntity)
         .include(this.userInclude)
         .validateDTOPipe(CreateUserDto, UpdateUserDto)
         .beforeSave<Partial<CreateUserDto>, UserEntity>(
            async (data: Partial<CreateUserDto>, { record: user, context }) => {
               if (context === 'create') {
                  await this.validateUserDto(data);
               } else {
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
               }

               if (data.password) {
                  data.password = await argon2.hash(data.password);
               }
            },
         )
         .beforeDelete((user: UserEntity) => {
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
         })
         .execute();
   }
}
