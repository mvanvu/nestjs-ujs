import { BaseService } from '@/core';
import { userService } from '@lib/constant/user';
import { ServieConstant } from '@lib/type';
import { Injectable } from '@nestjs/common';

@Injectable()
export class UserService extends BaseService {
   readonly options: { serviceConstant: ServieConstant } = { serviceConstant: userService };

   async signUp(data: any) {
      console.log(data, this);

      return data;
   }
}
