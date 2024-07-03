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
            const finalResponse: ApiFinalResponse =
               Is.object(response) && Is.object(response.meta)
                  ? { data: response?.data ?? null, meta: response.meta, message: response?.message ?? null }
                  : { data: response?.data ?? response ?? null, message: response?.message ?? null };

            if (isGateway()) {
               finalResponse.success = true;
            }

            return finalResponse;
         }),
      );
   }
}
