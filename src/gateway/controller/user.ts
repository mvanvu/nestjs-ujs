import { Body, Controller, Delete, Get, HttpStatus, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import {
   PaginationQueryDto,
   Public,
   ApiResultResponse,
   ParseMongoIdPipe,
   EntityResponse,
   Pagination,
   Permission,
} from '@lib/common';
import { CreateUserDto, UserSignInDto, UserSignUpDto, UserEntity, AuthEntity, UpdateUserDto } from '@lib/service/user';
import { BaseController, BaseClientProxy } from '../lib';
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
   @ApiResultResponse(() => UserEntity, { summary: 'Register a new user account' })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signUp'), { data });
   }

   @Public()
   @Post('signin')
   @ApiResultResponse(() => AuthEntity, { summary: 'Sign-in with the user account' })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signIn'), { data });
   }

   @Get()
   @Permission({ key: serviceConfig.get('user.permissions.user.read') })
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'Admin get list pagination of the users' })
   paginate(@Query() query: PaginationQueryDto): Promise<Pagination<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { query, CRUD: { method: 'read' } } });
   }

   @Get(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.read') })
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @Post()
   @Permission({ key: serviceConfig.get('user.permissions.user.create') })
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): Promise<EntityResponse<UserEntity>> {
      console.log({ data });
      return this.userProxy.send(userCRUDPattern, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.update') })
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'Admin update the user account' })
   update(@Param('id', ParseMongoIdPipe) id: string, @Body() data: UpdateUserDto): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { data, meta: { params: { id }, CRUD: { method: 'write' } } });
   }

   @Delete(':id')
   @Permission({ key: serviceConfig.get('user.permissions.user.delete') })
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'Admin delete an user account' })
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(userCRUDPattern, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }

   @Delete(':id')
   @ApiBearerAuth()
   @ApiResultResponse(() => UserEntity, { summary: 'The user delete his/her self' })
   deleteSelf(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<UserEntity>> {
      return this.userProxy.send(serviceConfig.get('user.patterns.deleteSelf'), { meta: { params: { id } } });
   }
}
