import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   PaginationQueryDto,
   ParseMongoIdPipe,
   User,
   CRUDClient,
   AuthTokenEntity,
   UserEntity,
   AuthEntity,
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
   PaginationResponse,
   EntityResponse,
   ApiEntityResponse,
   ApiPaginationResponse,
   Public,
   Permission,
   HttpCache,
} from '@gateway/@library';
import { serviceConfig } from '@metadata';

const { name, permissions, patterns } = serviceConfig.get('user');

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get userProxy(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   get userCRUD(): CRUDClient {
      return this.userProxy.createCRUD(patterns.userCRUD);
   }

   @Public()
   @Post('refresh-token')
   @ApiEntityResponse(AuthTokenEntity, {
      summary: 'Generate a new pair access/refresh token',
      statusCode: HttpStatus.OK,
   })
   refreshToken(@Body() data: AuthTokenDto): Promise<AuthTokenEntity> {
      return this.userProxy.send(patterns.refreshToken, data);
   }

   @Public()
   @Post('signup')
   @ApiEntityResponse(UserEntity, { summary: 'Register a new user account', statusCode: HttpStatus.CREATED })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(patterns.signUp, data);
   }

   @Public()
   @Post('activate')
   @ApiEntityResponse(UserEntity, {
      summary: 'Activate the account by pass a verification code',
      statusCode: HttpStatus.OK,
   })
   activateAccount(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(patterns.verifyAccount, data);
   }

   @Public()
   @Post('signin')
   @ApiEntityResponse(AuthEntity, { summary: 'Sign-in with the user account', statusCode: HttpStatus.OK })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(patterns.signIn, data);
   }

   @Public()
   @Post('send-reset-password')
   @ApiEntityResponse(Boolean, {
      summary: 'Send a verify reset password code to the user email',
      statusCode: HttpStatus.OK,
   })
   async sendResetPassword(@Body() data: SendResetPasswordCodeDto): Promise<true> {
      await this.userProxy.send(patterns.sendResetPasswordCode, data);

      // Always returns true for security perpose
      return true;
   }

   @Public()
   @Post('reset-password')
   @ApiEntityResponse(UserEntity, { summary: 'Activate the account by pass a verification code' })
   resetPassword(@Body() data: ResetPasswordDto): Promise<UserEntity> {
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
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<UserEntity>> {
      return this.userCRUD.paginate(query);
   }

   @Get(':id')
   @Permission({ key: permissions.user.read, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userCRUD.read(id);
   }

   @Post()
   @Permission({ key: permissions.user.create, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userCRUD.create(data);
   }

   @Patch(':id')
   @Permission({ key: permissions.user.update, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin update the user account' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userCRUD.update(id, data);
   }

   @Delete(':id')
   @Permission({ key: permissions.user.delete, adminScope: true })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin delete an user account' })
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userCRUD.delete(id);
   }

   @Delete()
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'The user delete his/her self' })
   deleteSelf(@User('id') id: string): Promise<EntityResponse<UserEntity>> {
      return this.userCRUD.delete(id);
   }
}
