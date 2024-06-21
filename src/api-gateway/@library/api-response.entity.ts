import { ApiProperty } from '@nestjs/swagger';

export class PaginationMeta {
   @ApiProperty()
   totalCount: number;

   @ApiProperty()
   page: number;

   @ApiProperty()
   limit: number;
}

export class PaginationResponse<TData> {
   @ApiProperty()
   success: true;

   @ApiProperty()
   data: TData[];

   @ApiProperty({ type: PaginationMeta })
   meta: PaginationMeta;
}

export class EntityResponse<TData> {
   @ApiProperty()
   success: true;

   @ApiProperty()
   data: TData;
}
