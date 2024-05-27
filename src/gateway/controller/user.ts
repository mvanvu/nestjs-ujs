import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   PaginationQueryDto,
   Public,
   ParseMongoIdPipe,
   Permission,
   ApiPaginationResponse,
   ApiEntityResponse,
} from '@lib/common';
import { CreateUserDto, UserSignInDto, UserSignUpDto, UserEntity, AuthEntity, UpdateUserDto } from '@lib/service/user';
import { BaseController, BaseClientProxy, PaginationResponse, EntityResponse } from '../lib';
import { serviceConfig } from '@config';
const userCRUDPattern: string = serviceConfig.get('user.patterns.userCRUD');

@ApiTags('Users')
@Controller('users')
export class UserController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(serviceConfig.get('user.name'));
   }

   @Public()
   @Post('signup')
   @ApiEntityResponse(UserEntity, { summary: 'Register a new user account' })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signUp'), { data });
   }

   @Public()
   @Post('signin')
   @ApiEntityResponse(AuthEntity, { summary: 'Sign-in with the user account' })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signIn'), { data });
   }

   @Get()
   @Permission({ key: serviceConfig.get('user.permissions.user.read') })
   @ApiBearerAuth()
   @ApiPaginationResponse(UserEntity, { summary: 'Admin get list pagination of the users' })
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Get(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.read') })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Post()
   @Permission({ key: serviceConfig.get('user.permissions.user.create') })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.update') })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin update the user account' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Delete(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.delete') })
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'Admin delete an user account' })
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }

   @Delete(':id')
   @ApiBearerAuth()
   @ApiEntityResponse(UserEntity, { summary: 'The user delete his/her self' })
   deleteSelf(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(serviceConfig.get('user.patterns.deleteSelf'), { meta: { params: { id } } });
   }
}
