import { IProperty } from '@lib/decorator';

export class PaginationMeta {
   @IProperty()
   totalCount: number;

   @IProperty()
   page: number;

   @IProperty()
   limit: number;
}
