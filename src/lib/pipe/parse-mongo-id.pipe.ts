import { ThrowException } from '@lib/exception';
import { Is } from '@mvanvu/ujs';
import { Injectable, PipeTransform } from '@nestjs/common';

@Injectable()
export class ParseMongoIdPipe implements PipeTransform<any, string> {
   transform(value: any): string {
      if (!Is.string(value) || !value.match(/^[0-9a-fA-F]{24}$/)) {
         ThrowException('Invalid Mongo ID');
      }

      return value;
   }
}
