import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import { CreateUserDto, UserSignInDto, UserSignUpDto, UserEntity, AuthEntity, UpdateUserDto } from '@lib/service/user';
import { ServiceExecuteResult } from '@lib/type';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { serviceConfig } from '@config';

@Controller()
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @MessagePattern(serviceConfig.get('user.patterns.signUp'))
   signUp(@Payload() data: UserSignUpDto): Promise<UserEntity> {
      return this.userService.execute(data);
   }

   @MessagePattern(serviceConfig.get('user.patterns.signIn'))
   signIn(@Payload() data: UserSignInDto): Promise<AuthEntity> {
      return this.userService.execute(data);
   }

   @MessagePattern(serviceConfig.get('user.patterns.verify'))
   verify(@Payload() token: string): Promise<UserEntity> {
      return this.userService.execute(token);
   }

   @MessagePattern(serviceConfig.get('user.patterns.paginateUser'))
   @MessagePattern(serviceConfig.get('user.patterns.readUser'))
   read(): Promise<UserEntity> {
      return this.userService.execute();
   }

   @MessagePattern(serviceConfig.get('user.patterns.createUser'))
   @MessagePattern(serviceConfig.get('user.patterns.updateUser'))
   write(@Payload() data: CreateUserDto | UpdateUserDto): ServiceExecuteResult<UserEntity> {
      return this.userService.execute(data);
   }

   @MessagePattern(serviceConfig.get('user.patterns.deleteUser'))
   delete(): Promise<UserEntity> {
      return this.userService.execute();
   }
}
