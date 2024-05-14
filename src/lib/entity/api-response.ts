import { ApiProperty } from '@nestjs/swagger';

export class PaginationResponse<TData> {
   @ApiProperty()
   success: true;

   data: TData[];

   @ApiProperty({ example: { totalCount: 1, page: 1, limit: 25 } })
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
}

export class EntityResponse<TData> {
   @ApiProperty()
   success: true;

   data: TData;

   @ApiProperty()
   meta: Record<string, any>;
}

export class Pagination<TData> {
   data: TData[];
   meta: {
      totalCount: number;
      page: number;
      limit: number;
   };
}
