import { Is } from '@mvanvu/ujs';
import { CallHandler, ExecutionContext, Injectable, NestInterceptor } from '@nestjs/common';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

@Injectable()
export class TransformInterceptor implements NestInterceptor {
   intercept(context: ExecutionContext, next: CallHandler): Observable<any> {
      return next
         .handle()
         .pipe(
            map((response) =>
               Is.object(response, { suitable: false, rules: { meta: 'object' } })
                  ? { success: true, data: response.data, meta: response.meta }
                  : { success: true, data: response },
            ),
         );
   }
}
