import { Controller, Inject, RequestMethod } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from './user.service';
import { IBody, IRoute } from '@lib/decorator';
import { userConfig } from './user.config';
import { UserSignUpDto } from './dto';
import { MessageData, PaginationResult } from '@lib';
import { UserEntity } from '@service/user/user.entity';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userConfig: UserService;

   @IRoute({
      pattern: userConfig.patterns.signUp,
      route: { method: RequestMethod.POST, path: 'signup', public: true },
   })
   signUp(@IBody() dto: UserSignUpDto): Promise<UserEntity> {
      return this.userConfig.execute(userConfig.patterns.signUp, dto);
   }

   @IRoute({
      pattern: userConfig.patterns.paginate,
      route: { method: RequestMethod.GET },
   })
   paginate(@IBody() data: MessageData): Promise<PaginationResult<UserEntity>> {
      return this.userConfig.execute(userConfig.patterns.paginate, data);
   }
}
