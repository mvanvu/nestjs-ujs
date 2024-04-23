import { Controller, Inject, RequestMethod } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from './user.service';
import { IRoute } from '@lib/decorator';
import { userConfig } from './user.config';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userConfig: UserService;

   @IRoute({
      pattern: userConfig.patterns.signUp,
      route: { method: RequestMethod.POST, path: 'signup', public: true },
   })
   signUp(): Promise<any> {
      return this.userConfig.execute(userConfig.patterns.signUp, 'The new DTO data');
   }

   @IRoute({
      pattern: userConfig.patterns.paginate,
      route: { method: RequestMethod.GET },
   })
   paginate(): Promise<any> {
      return this.userConfig.execute(userConfig.patterns.paginate, 'The new DTO data');
   }
}
