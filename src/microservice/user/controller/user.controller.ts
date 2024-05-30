import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import { UserSignInDto, UserSignUpDto, UserEntity, AuthEntity } from '@lib/service';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { serviceConfig } from '@config';
const patterns = serviceConfig.get('user.patterns');

@Controller()
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @MessagePattern(patterns.roleCRUD)
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

   @MessagePattern(patterns.verify)
   verify(@Payload() token: string): Promise<UserEntity> {
      return this.userService.verify(token);
   }

   @MessagePattern(patterns.deleteSelf)
   deleteSelf(): Promise<UserEntity> {
      return this.userService.deleteSelf(this.userService.meta.get<string>('headers.user.id'));
   }
}
