import { $Enums } from '.prisma/user';
import { EnumSchema, StringSchema, DateSchema, IDSchema, NameSchema } from '../decorator/schema';

export class RoleEntity {
   @IDSchema()
   id: string;

   @EnumSchema(Object.values($Enums.AvailableStatus))
   status: $Enums.AvailableStatus;

   @NameSchema()
   name: string;

   @StringSchema({ nullable: true })
   description?: string;

   @DateSchema()
   createdAt: Date;

   @DateSchema()
   updatedAt: Date;

   @StringSchema({ isArray: 'unique' })
   permissions: string[];
}
