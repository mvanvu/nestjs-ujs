import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import { UserSignInDto, UserSignUpDto, UserEntity, AuthEntity, AuthTokenDto, AuthTokenEntity } from '@lib/service';
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
   verifyToken(@Payload() dto: AuthTokenDto): Promise<UserEntity> {
      return this.userService.verifyToken(dto.token);
   }

   @MessagePattern(patterns.refreshToken)
   refreshToken(@Payload() dto: AuthTokenDto): Promise<AuthTokenEntity> {
      return this.userService.refreshToken(dto.token);
   }
}
