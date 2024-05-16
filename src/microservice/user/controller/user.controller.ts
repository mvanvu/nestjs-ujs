import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import { CreateUserDto, UserSignInDto, UserSignUpDto, UserEntity, AuthEntity, UpdateUserDto } from '@lib/service/user';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { serviceConfig } from '@config';
import { CRUDResult } from '@lib/common';

@Controller()
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @MessagePattern(serviceConfig.get('user.patterns.userCRUD'))
   CRUD(): Promise<CRUDResult<UserEntity>> {
      return this.userService.executeCRUD(CreateUserDto, UpdateUserDto);
   }

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
}
