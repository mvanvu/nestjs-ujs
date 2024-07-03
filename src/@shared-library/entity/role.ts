import { $Enums } from '.prisma/user';
import { EnumSchema, StringSchema } from '../decorator/schema';

export class RoleEntity {
   @StringSchema()
   id: string;

   @EnumSchema(Object.values($Enums.AvailableStatus))
   status: $Enums.AvailableStatus;

   @StringSchema()
   name: string;

   @StringSchema()
   description: string;

   @StringSchema({ format: 'date-time' })
   createdAt?: Date;

   @StringSchema({ format: 'date-time' })
   updatedAt?: Date;

   @StringSchema({ isArray: true })
   permissions: string[];
}
