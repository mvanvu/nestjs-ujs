import { CanActivate, ExecutionContext, Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';

@Injectable()
export class UserGuard implements CanActivate {
   // eslint-disable-next-line @typescript-eslint/no-unused-vars
   canActivate(context: ExecutionContext): boolean | Promise<boolean> | Observable<boolean> {
      return true;
   }
}
