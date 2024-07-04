import { AvailableStatus } from '.prisma/order';
import { EnumSchema, IPartialType, NumberSchema, StringSchema, IDSchema } from '@shared-library';

export class CreateTableDto {
   @IDSchema()
   restaurantId: string;

   @EnumSchema(Object.values(AvailableStatus), { optional: true })
   status?: AvailableStatus;

   @NumberSchema({ min: 1 })
   number: number;

   @StringSchema({ optional: true, empty: false })
   area?: string;
}

export class UpdateTableDto extends IPartialType(CreateTableDto) {}
