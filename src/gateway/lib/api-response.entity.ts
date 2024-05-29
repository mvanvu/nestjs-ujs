import { ApiProperty } from '@nestjs/swagger';

export class PaginationMetaResponse {
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

   data: TData[];

   @ApiProperty({ type: () => PaginationMetaResponse })
   meta: PaginationMetaResponse;
}

export class EntityResponse<TData> {
   @ApiProperty()
   success: true;

   data: TData;
}
