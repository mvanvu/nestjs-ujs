import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   PaginationQueryDto,
   ParseTypePipe,
   CRUDClient,
   AuthTokenEntity,
   UserEntity,
   AuthEntity,
   EntityResult,
   PaginationResult,
   ThrowException,
   Language,
} from '@shared-library';
import {
   CreateUserDto,
   UserSignInDto,
   UserSignUpDto,
   UpdateUserDto,
   AuthTokenDto,
   SendResetPasswordCodeDto,
   ResetPasswordDto,
} from '@microservice/user/dto';
import {
   BaseClientProxy,
   ApiEntityResponse,
   ApiPaginationResponse,
   Public,
   Permission,
   HttpCache,
   User,
} from '@gateway/@library';
import { serviceConfig } from '@metadata';

const { name, permissions, patterns } = serviceConfig.get('user');

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   @Inject(Language) private readonly language: Language;

   get userProxy(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   get userCRUD(): CRUDClient {
      return this.userProxy.createCRUD(patterns.userCRUD, { entityResponse: UserEntity });
   }

   @Public()
   @Post('refresh-token')
   @ApiEntityResponse(AuthTokenEntity, {
      summary: 'Generate a new pair access/refresh token',
      statusCode: HttpStatus.OK,
   })
   refreshToken(@Body() data: AuthTokenDto): Promise<EntityResult<AuthTokenEntity>> {
      return this.userProxy.send(patterns.refreshToken, data);
   }

   @Public()
   @Post('verify-token')
   @ApiEntityResponse(UserEntity, {
      summary: 'Validate the access/refresh token',
      statusCode: HttpStatus.OK,
   })
   verifyToken(@Body() data: AuthTokenDto): Promise<EntityResult<UserEntity>> {
      return this.userProxy.send(patterns.verifyToken, data);
   }

   @Public()
   @Post('signup')
   @ApiEntityResponse(UserEntity, { summary: 'Register a new user account', statusCode: HttpStatus.CREATED })
   signUp(@Body() data: UserSignUpDto): Promise<EntityResult<UserEntity>> {
      return this.userProxy.send(patterns.signUp, data);
   }

   @Public()
   @Post('activate')
   @ApiEntityResponse(UserEntity, {
      summary: 'Activate the account by pass a verification code',
      statusCode: HttpStatus.OK,
   })
   activateAccount(@Body() data: UserSignUpDto): Promise<EntityResult<UserEntity>> {
      return this.userProxy.send(patterns.verifyAccount, data);
   }

   @Public()
   @Post('signin')
   @ApiEntityResponse(AuthEntity, { summary: 'Sign-in with the user account', statusCode: HttpStatus.OK })
   signIn(@Body() data: UserSignInDto): Promise<EntityResult<AuthEntity>> {
      return this.userProxy.send(patterns.signIn, data);
   }

   @Public()
   @Post('send-reset-password')
   @ApiEntityResponse(Boolean, {
      summary: 'Send a verify reset password code to the user email',
      statusCode: HttpStatus.OK,
   })
   async sendResetPassword(@Body() data: SendResetPasswordCodeDto): Promise<true> {
      // Always returns true for security purpose
      await this.userProxy.send(patterns.sendResetPasswordCode, data);

      return true;
   }

   @Public()
   @Post('reset-password')
   @ApiEntityResponse(UserEntity, { summary: 'Activate the account by pass a verification code' })
   resetPassword(@Body() data: ResetPasswordDto): Promise<EntityResult<UserEntity>> {
      return this.userProxy.send(patterns.resetPassword, data);
   }

   @Get('me')
   @ApiBearerAuth()
   @HttpCache({ withUserIdPrefix: true })
   @ApiEntityResponse(UserEntity, { summary: 'Get the detail of the logged user' })
   me(@User() user: UserEntity): UserEntity {
      return user;
   }

   @Get()
   @Permission({ key: permissions.user.read })
   @ApiBearerAuth()
   @ApiPaginationResponse(UserEntity, { summary: 'Admin get list pagination of the users' })
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<UserEntity>> {
      return this.userCRUD.paginate(query);
   }

   @Get(':id')
   @Permission({ key: permissions.user.read, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseTypePipe('mongoId')) id: string): Promise<EntityResult<UserEntity>> {
      return this.userCRUD.read(id);
   }

   @Post()
   @Permission({ key: permissions.user.create, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): Promise<EntityResult<UserEntity>> {
      return this.userCRUD.create(data);
   }

   @Patch(':id')
   @Permission({ key: permissions.user.update, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin update the user account' })
   async update(
      @Param('id', ParseTypePipe('mongoId')) id: string,
      @Body() data: UpdateUserDto,
      @User() author: UserEntity,
   ): Promise<EntityResult<UserEntity>> {
      // Verify permission
      const { data: user } = await this.userCRUD.read<UserEntity>(id);
      const isSelf = author.id === user.id;

      if (isSelf && data.status) {
         ThrowException(this.language._('USER_UPDATE_SELF_STATUS_DENIED'), HttpStatus.FORBIDDEN);
      }

      if (data.groupId && isSelf && !author.isRoot) {
         ThrowException(this.language._('USER_UPDATE_SELF_GROUP_DENIED'), HttpStatus.FORBIDDEN);
      }

      // Check the current user is the author or editor of the target user, then will can update
      const isGranter = user.createdBy === author.id || user.updatedBy === author.id;

      if (!isGranter && !(isSelf && author.isRoot)) {
         const compare = author.compare(user, permissions.user.update);

         if (compare === 0) {
            ThrowException(this.language._('USER_UPDATE_SAME_PERMIT_DENIED'), HttpStatus.FORBIDDEN);
         }

         if (compare === -1) {
            ThrowException(this.language._('USER_UPDATE_GREATER_PERMIT_DENIED'), HttpStatus.FORBIDDEN);
         }
      }

      return this.userCRUD.update(id, data);
   }

   @Delete(':id')
   @Permission({ key: permissions.user.delete, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin delete an user account' })
   async delete(
      @Param('id', ParseTypePipe('mongoId')) id: string,
      @User() author: UserEntity,
   ): Promise<EntityResult<UserEntity>> {
      // Verify permission
      const { data: user } = await this.userCRUD.read<UserEntity>(id);
      const isSelf = author.id === user.id;

      if (isSelf) {
         ThrowException(this.language._('USER_DELETE_SELF_DENIED'));
      }

      // Check the current user is the author or editor of the target user, then will can delete
      const isGranter = user.createdBy === author.id || user.updatedBy === author.id;

      if (!isGranter) {
         const compare = author.compare(user, permissions.user.delete);

         if (compare === 0) {
            ThrowException(this.language._('USER_DELETE_SAME_PERMIT_DENIED'), HttpStatus.FORBIDDEN);
         }

         if (compare === -1) {
            ThrowException(this.language._('USER_DELETE_GREATER_PERMIT_DENIED'), HttpStatus.FORBIDDEN);
         }
      }

      return this.userCRUD.delete(id);
   }

   @Delete()
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'The user delete his/her self' })
   deleteSelf(@User('id') id: string): Promise<EntityResult<UserEntity>> {
      return this.userCRUD.delete(id);
   }
}
