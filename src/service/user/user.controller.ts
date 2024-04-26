import { Controller, HttpStatus, Inject, Param, RequestMethod } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from './user.service';
import { IBody, IRoute } from '@lib/decorator';
import { userConfig } from './user.config';
import { UserSignInDto, UserSignUpDto } from './dto';
import { MessageData, ServiceExecuteResult } from '@lib';
import { UserEntity, SwaggerPaginationUserEntity, AuthEntity } from './user.entity';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userConfig: UserService;

   @IRoute({
      pattern: userConfig.patterns.signUp,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'signup', public: true },
      swagger: { summary: 'Register a new user account', responseType: UserEntity },
   })
   signUp(@IBody() dto: UserSignUpDto): ServiceExecuteResult<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.signUp, dto);
   }

   @IRoute({
      pattern: userConfig.patterns.signIn,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'signin', public: true },
      swagger: { summary: 'Sing-in with the user account', responseType: AuthEntity },
   })
   signIn(@IBody() dto: UserSignInDto): ServiceExecuteResult<AuthEntity> {
      return this.userConfig.execute(userConfig.patterns.signIn, dto);
   }

   @IRoute({ pattern: userConfig.patterns.verify })
   verify(@IBody() token: string): ServiceExecuteResult<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.verify, token);
   }

   @IRoute({
      pattern: userConfig.patterns.paginate,
      route: { method: RequestMethod.GET },
      swagger: {
         summary: 'Get list pagination of the users',
         responseType: SwaggerPaginationUserEntity,
      },
   })
   paginate(@IBody() data: MessageData): ServiceExecuteResult<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.paginate, data);
   }

   @IRoute({
      pattern: userConfig.patterns.read,
      route: { method: RequestMethod.GET, path: ':id' },
      swagger: { summary: 'Get the detail of user account', responseType: UserEntity },
   })
   read(@Param('id') id: string, @IBody() data: MessageData): ServiceExecuteResult<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.read, data, { params: { id } });
   }

   @IRoute({
      pattern: userConfig.patterns.delete,
      route: { method: RequestMethod.DELETE, path: ':id' },
      swagger: { summary: 'Delete an user account', responseType: UserEntity },
   })
   delete(@Param('id') id: string, @IBody() data: MessageData): ServiceExecuteResult<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.delete, data, { params: { id } });
   }
}
