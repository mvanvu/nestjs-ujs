import { Controller, Inject } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';
import { UserService } from './user.service';
import { IRoute } from '@lib/decorator';
import { userService } from '@lib/constant/user';

@ApiTags('Users')
@Controller('users')
export class UserController {
   @Inject(UserService) readonly userService: UserService;

   @IRoute({ pattern: userService.patterns.signUp, route: { method: 'POST', path: 'signup' } })
   signUp(): Promise<any> {
      return this.userService.execute(userService.patterns.signUp, 'The new DTO data');
   }
}
