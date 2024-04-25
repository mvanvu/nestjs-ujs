import {
   BaseService,
   ServiceOptions,
   CRUDService,
   PaginationResult,
   FieldsException,
   MessageData,
   ThrowException,
   appConfig,
} from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Prisma } from '.prisma/user';
import { AuthEntity, UserEntity } from './user.entity';
import { userConfig } from './user.config';
import { UserSignInDto, UserSignUpDto } from './dto';
import * as argon2 from 'argon2';
import { DateTime, Hash, Is } from '@mvanvu/ujs';

@Injectable()
export class UserService extends BaseService {
   readonly options: ServiceOptions = { config: userConfig };

   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService['user'], any, Prisma.UserSelect> {
      return new CRUDService({
         model: this.prisma.user,
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
      const user = await this.prisma.user.findUnique({ where: { id } });

      if (!user || user.status !== this.prisma.enums.UserStatus.ACTIVE) {
         ThrowException('Invalid credentials');
      }

      return new UserEntity(user);
   }

   async paginate({ meta }: MessageData): Promise<PaginationResult<UserEntity>> {
      return this.createCRUDService().paginate(meta.get('query'));
   }
}
