import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { PaginationQueryDto, ParseMongoIdPipe, IUser } from '@lib/common';
import {
   CreateUserDto,
   UserSignInDto,
   UserSignUpDto,
   UserEntity,
   AuthEntity,
   UpdateUserDto,
   AuthTokenDto,
   AuthTokenEntity,
   AuthUpdateResetPasswordCodeDto,
} from '@lib/service/user';
import {
   BaseController,
   BaseClientProxy,
   PaginationResponse,
   EntityResponse,
   ApiEntityResponse,
   ApiPaginationResponse,
   Public,
   Permission,
} from '@gateway/lib';
import { serviceConfig } from '@metadata';

const { name, permissions, patterns } = serviceConfig.get('user');

@ApiTags('Users')
@Controller('users')
export class UserController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Public()
   @Post('refresh-token')
   @ApiEntityResponse(AuthTokenEntity, { summary: 'Generate a new pair access/refresh token' })
   refreshToken(@Body() data: AuthTokenDto): Promise<AuthTokenEntity> {
      return this.userProxy.send(patterns.refreshToken, { data });
   }

   @Public()
   @Post('signup')
   @ApiEntityResponse(UserEntity, { summary: 'Register a new user account' })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(patterns.signUp, { data });
   }

   @Public()
   @Post('signin')
   @ApiEntityResponse(AuthEntity, { summary: 'Sign-in with the user account' })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(patterns.signIn, { data });
   }

   @Public()
   @Post('reset-password')
   @ApiEntityResponse(Boolean, { summary: 'Send a verify reset password code to the user email' })
   async resetPassword(@Body() data: AuthUpdateResetPasswordCodeDto): Promise<true> {
      await this.userProxy.send(patterns.updateResetPasswordCode, { data });

      // Always returns true for security perpose
      return true;
   }

   @Get('me')
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Get the detail of the logged user' })
   me(@IUser() user: UserEntity): UserEntity {
      return user;
   }

   @Get()
   @Permission({ key: permissions.user.read })
   @ApiBearerAuth()
   @ApiPaginationResponse(UserEntity, { summary: 'Admin get list pagination of the users' })
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Get(':id')
   @Permission({ key: permissions.user.read })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Post()
   @Permission({ key: permissions.user.create })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @Permission({ key: permissions.user.update })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin update the user account' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Delete(':id')
   @Permission({ key: permissions.user.delete })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin delete an user account' })
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }

   @Delete()
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'The user delete his/her self' })
   deleteSelf(@IUser('id') id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(patterns.userCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
