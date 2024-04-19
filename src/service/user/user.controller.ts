import { Controller, Inject, RequestMethod } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from './user.service';
import { IRoute } from '@lib/decorator';
import { userService } from '@lib/constant/user';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @IRoute({
      pattern: userService.patterns.signUp,
      route: { method: RequestMethod.POST, path: 'signup', public: true },
   })
   signUp(): Promise<any> {
      return this.userService.execute(userService.patterns.signUp, 'The new DTO data');
   }

   @IRoute({
      pattern: userService.patterns.paginate,
      route: { method: RequestMethod.GET },
   })
   paginate(): Promise<any> {
      return this.userService.execute(userService.patterns.paginate, 'The new DTO data');
   }
}
