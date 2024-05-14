import { Body, Controller, Delete, Get, HttpStatus, Param, Post, Query } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { ServiceExecuteResult, PaginationQueryDto, Public, ApiResultResponse, ParseMongoIdPipe } from '@lib';
import { CreateUserDto, UserSignInDto, UserSignUpDto, UserEntity, AuthEntity } from '@lib/service/user';
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
   @ApiResultResponse(UserEntity, { summary: 'Register a new user account' })
   signUp(@Body() data: UserSignUpDto): Promise<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signUp'), { data });
   }

   @Public()
   @Post('signin')
   @ApiResultResponse(AuthEntity, { summary: 'Sign-in with the user account' })
   signIn(@Body() data: UserSignInDto): Promise<AuthEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.signIn'), { data });
   }

   @Get()
   @ApiResultResponse(UserEntity, { summary: 'Admin get list pagination of the users' })
   paginate(@Query() query: PaginationQueryDto): Promise<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.readUser'), { meta: { query } });
   }

   @Get(':id')
   @ApiResultResponse(UserEntity, { summary: 'Admin get the detail of user account' })
   read(@Param('id', ParseMongoIdPipe) id: string): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.readUser'), { meta: { params: { id } } });
   }

   @Post()
   @ApiResultResponse(UserEntity, { summary: 'Admin create a new user account', statusCode: HttpStatus.CREATED })
   create(@Body() data: CreateUserDto): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.writeUser'), { data });
   }

   @Delete(':id')
   @ApiResultResponse(UserEntity, { summary: 'Admin delete an user account' })
   delete(@Param('id', ParseMongoIdPipe) id: string): ServiceExecuteResult<UserEntity> {
      return this.userProxy.send(serviceConfig.get('user.patterns.deleteUser'), { meta: { params: { id } } });
   }
}
