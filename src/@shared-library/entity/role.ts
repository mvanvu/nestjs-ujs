import { $Enums } from '.prisma/user';
import { EnumSchema, StringSchema, DateSchema } from '../decorator/schema';

export class RoleEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values($Enums.AvailableStatus))
   status: $Enums.AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema()
   description: string;

   @DateSchema()
   createdAt?: Date;

   @DateSchema()
   updatedAt?: Date;

   @StringSchema({ isArray: true })
   permissions: string[];
}
