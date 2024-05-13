import { Body, Controller, Delete, Get, HttpStatus, Param, Post, Query } from '@nestjs/common';
import { ApiProperty, ApiResponse, ApiTags } from '@nestjs/swagger';
import { ServiceExecuteResult, PaginationQueryDto, Public } from '@lib';
import {
   CreateUserDto,
   UserSignInDto,
   UserSignUpDto,
   UserEntity,
   AuthEntity,
   PaginationUserEntity,
} from '@lib/service/user';
import { BaseController, BaseClientProxy } from '../lib';
import { serviceConfig } from '@config';

@ApiTags('Users')
@Controller('users')
export class UserController extends BaseController {
   get userProxy(): BaseClientProxy {
      return this.createClientProxy(serviceConfig.get('user.proxy'));
   }

   @Public()
   @Post('signup')
   @ApiProperty({ description: 'Register a new user account' })
   @ApiResponse({ status: HttpStatus.OK, type: UserEntity })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signUp'), { data });
   }

   @Public()
   @Post('signin')
   @ApiProperty({ description: 'Sign-in with the user account' })
   @ApiResponse({ status: HttpStatus.OK, type: AuthEntity })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signIn'), { data });
   }

   @Get()
   @ApiProperty({ description: 'Admin get list pagination of the users' })
   @ApiResponse({ status: HttpStatus.OK, type: PaginationUserEntity })
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationUserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.paginateUser'), { meta: { query } });
   }

   @Get(':id')
   @ApiProperty({ description: 'Admin get the detail of user account' })
   @ApiResponse({ status: HttpStatus.OK, type: UserEntity })
   read(@Param('id') id: string): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.readUser'), { meta: { params: { id } } });
   }

   @Post()
   @ApiProperty({ description: 'Admin create a new user account' })
   @ApiResponse({ status: HttpStatus.OK, type: UserEntity })
   create(@Body() data: CreateUserDto): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.createUser'), { data });
   }

   @Delete(':id')
   @ApiProperty({ description: 'Admin delete an user account' })
   @ApiResponse({ status: HttpStatus.OK, type: UserEntity })
   delete(@Param('id') id: string): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.deleteUser'), { meta: { params: { id } } });
   }
}
