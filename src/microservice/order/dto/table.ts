import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, NumberSchema, StringSchema } from '@shared-library';

export class CreateTableDto {
   @StringSchema({ format: 'mongoId' })
   restaurantId: string;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NumberSchema({ min: 1 })
   number: number;

   @StringSchema({ optional: true, notEmpty: true })
   area?: string;
}

export class UpdateTableDto extends IPartialType(CreateTableDto) {}
