import { isGateway } from '@metadata';
import { Is } from '@mvanvu/ujs';
import { CallHandler, ExecutionContext, Injectable, NestInterceptor } from '@nestjs/common';
import { ApiFinalResponse } from '../type/common';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

@Injectable()
export class TransformInterceptor implements NestInterceptor {
   intercept(context: ExecutionContext, next: CallHandler): Observable<any> {
      return next.handle().pipe(
         map((response) => {
            const finalResponse: ApiFinalResponse = Is.object(response, { suitable: false, rules: { meta: 'object' } })
               ? { data: response?.data ?? null, meta: response.meta }
               : { data: response?.data ?? response ?? null };

            if (isGateway()) {
               finalResponse.success = true;
            }

            return finalResponse;
         }),
      );
   }
}
