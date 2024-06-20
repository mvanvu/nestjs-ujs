import { ThrowException } from '../exception/throw';
import { Is } from '@mvanvu/ujs';
import { Injectable, PipeTransform } from '@nestjs/common';

@Injectable()
export class ParseMongoIdPipe implements PipeTransform<any, string> {
   transform(value: any): string {
      if (!Is.mongoId(value)) {
         ThrowException(`The value(${value}) is not a Mongo Object ID`);
      }

      return value;
   }
}
