import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { TableEntity, CreateTableDto, UpdateTableDto } from '@lib/service/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/tables')
export class OrderTableController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get orderClient(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   @ApiPaginationResponse(TableEntity, { summary: 'Get list pagination of order tables' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<TableEntity>> {
      return this.orderClient.send(patterns.tableCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(TableEntity, { summary: 'Get detail of the table' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<TableEntity>> {
      return this.orderClient.send(patterns.tableCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(TableEntity, { summary: 'Create a new table', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateTableDto): Promise<EntityResponse<TableEntity>> {
      return this.orderClient.send(patterns.tableCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @ApiEntityResponse(TableEntity, { summary: 'Update a table' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateTableDto,
   ): Promise<EntityResponse<TableEntity>> {
      return this.orderClient.send(patterns.tableCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @ApiEntityResponse(TableEntity, { summary: 'Delete a table' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<TableEntity>> {
      return this.orderClient.send(patterns.tableCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
