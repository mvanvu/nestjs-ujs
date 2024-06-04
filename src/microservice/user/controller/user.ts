import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import { UserSignInDto, UserSignUpDto, UserEntity, AuthEntity } from '@lib/service';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { serviceConfig } from '@metadata';
const patterns = serviceConfig.get('user.patterns');

@Controller()
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @MessagePattern(patterns.userCRUD)
   executeCRUD(): Promise<CRUDResult<UserEntity>> {
      return this.userService.executeCRUD();
   }

   @MessagePattern(patterns.signUp)
   signUp(@Payload() data: UserSignUpDto): Promise<UserEntity> {
      return this.userService.signUp(data);
   }

   @MessagePattern(patterns.signIn)
   signIn(@Payload() data: UserSignInDto): Promise<AuthEntity> {
      return this.userService.signIn(data);
   }

   @MessagePattern(patterns.verifyToken)
   verifyToken(@Payload() token: string): Promise<UserEntity> {
      return this.userService.verifyToken(token);
   }

   @MessagePattern(patterns.verifyToken)
   refreshToken(@Payload() token: string): Promise<AuthEntity> {
      return this.userService.refreshToken(token);
   }
}
