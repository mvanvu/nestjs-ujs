import { Controller, Inject } from '@nestjs/common';
import { UserService } from '../provider/user.service';
import {
   UserSignInDto,
   UserSignUpDto,
   AuthTokenDto,
   SendResetPasswordCodeDto,
   VerifyAccountDto,
   ResetPasswordDto,
} from '../dto';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { AuthEntity, AuthTokenEntity, CRUDResult, DataMetaResult, UserEntity } from '@shared-library';
import { serviceConfig } from '@metadata';
const patterns = serviceConfig.get('user.patterns');

@Controller()
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @MessagePattern(patterns.userCRUD)
   executeCRUD(): Promise<CRUDResult<UserEntity>> {
      return this.userService.createCRUDService().execute();
   }

   @MessagePattern(patterns.signUp)
   signUp(@Payload() data: UserSignUpDto): Promise<DataMetaResult<UserEntity>> {
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

   @MessagePattern(patterns.verifyAccount)
   verifyAccount(@Payload() dto: VerifyAccountDto): Promise<false | UserEntity> {
      return this.userService.verifyAccount(dto.code);
   }

   @MessagePattern(patterns.resetPassword)
   resetPassword(@Payload() dto: ResetPasswordDto): Promise<false | UserEntity> {
      return this.userService.resetPassword(dto);
   }

   @MessagePattern(patterns.refreshToken)
   refreshToken(@Payload() dto: AuthTokenDto): Promise<AuthTokenEntity> {
      return this.userService.refreshToken(dto.token);
   }

   @MessagePattern(patterns.sendResetPasswordCode)
   updateResetPasswordCode(@Payload() dto: SendResetPasswordCodeDto): Promise<false | DataMetaResult<UserEntity>> {
      return this.userService.updateResetPasswordCode(dto.email);
   }
}
