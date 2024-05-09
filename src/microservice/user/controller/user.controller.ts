import { Controller, HttpStatus, Inject, Param, RequestMethod } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from '../provider/user.service';
import { IData, IQuery, IRoute } from '@lib/decorator';
import { userConfig } from '../user.config';
import { CreateUserDto, UserSignInDto, UserSignUpDto } from '../dto';
import { PaginationQueryDto, ServiceExecuteResult } from '@lib';
import { UserEntity, SwaggerPaginationUserEntity, AuthEntity } from '../entity/user';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @IRoute({
      pattern: userConfig.patterns.signUp,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'signup', public: true },
      swagger: { summary: 'Register a new user account', responseType: UserEntity },
   })
   signUp(@IData() data: UserSignUpDto): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.signUp, { data });
   }

   @IRoute({
      pattern: userConfig.patterns.signIn,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'signin', public: true },
      swagger: { summary: 'Sing-in with the user account', responseType: AuthEntity },
   })
   signIn(@IData() data: UserSignInDto): ServiceExecuteResult<AuthEntity> {
      return this.userService.execute(userConfig.patterns.signIn, { data });
   }

   @IRoute({ pattern: userConfig.patterns.verify })
   verify(@IData() token: string): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.verify, { data: token });
   }

   @IRoute({
      pattern: userConfig.patterns.paginateUser,
      route: { method: RequestMethod.GET },
      swagger: {
         summary: 'Get list pagination of the users',
         responseType: SwaggerPaginationUserEntity,
      },
   })
   paginate(@IQuery() query: PaginationQueryDto): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.paginateUser, { meta: { query } });
   }

   @IRoute({
      pattern: userConfig.patterns.readUser,
      route: { method: RequestMethod.GET, path: ':id' },
      swagger: { summary: 'Get the detail of user account', responseType: UserEntity },
   })
   read(@Param('id') id: string): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.readUser, { meta: { params: { id } } });
   }

   @IRoute({
      pattern: userConfig.patterns.createUser,
      route: { method: RequestMethod.POST },
      swagger: { summary: 'Admin create a new user account', responseType: UserEntity },
   })
   create(@IData() data: CreateUserDto): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.createUser, { data });
   }

   @IRoute({
      pattern: userConfig.patterns.deleteUser,
      route: { method: RequestMethod.DELETE, path: ':id' },
      swagger: { summary: 'Delete an user account', responseType: UserEntity },
   })
   delete(@Param('id') id: string): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(userConfig.patterns.deleteUser, { meta: { params: { id } } });
   }
}
