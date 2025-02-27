import { Schema } from '@mvanvu/ujs';
import { PaginationQueryDto } from '@shared-library';

export class PostPaginationQueryDto extends PaginationQueryDto {
   @(Schema.content().optional().nullable(false).decorate())
   tag: string;
}
