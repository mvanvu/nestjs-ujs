import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   EntityResponse,
   PaginationResponse,
} from '@gateway/lib';
import { PaginationQueryDto, ParseMongoIdPipe } from '@lib/common';
import { StaffEntity, CreateStaffDto, UpdateStaffDto } from '@lib/service/order';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Order staffs')
@Controller('order/staffs')
export class OrderStaffController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get orderProxy(): BaseClientProxy {
      return this.proxy.create(name);
   }

   @ApiPaginationResponse(StaffEntity, { summary: 'Get list pagination of restaurant staffs' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<StaffEntity>> {
      return this.orderProxy.send(patterns.staffCRUD, { meta: { query, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Get detail of the staff' })
   @Get(':id')
   read(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<StaffEntity>> {
      return this.orderProxy.send(patterns.staffCRUD, { meta: { params: { id }, CRUD: { method: 'read' } } });
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Create a new staff', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateStaffDto): Promise<EntityResponse<StaffEntity>> {
      return this.orderProxy.send(patterns.staffCRUD, { data, meta: { CRUD: { method: 'write' } } });
   }

   @Patch(':id')
   @ApiEntityResponse(StaffEntity, { summary: 'Update a staff' })
   update(
      @Param('id', ParseMongoIdPipe) id: string,
      @Body() data: UpdateStaffDto,
   ): Promise<EntityResponse<StaffEntity>> {
      return this.orderProxy.send(patterns.staffCRUD, {
         data,
         meta: { params: { id }, CRUD: { method: 'write' } },
      });
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Delete a table' })
   @Delete(':id')
   delete(@Param('id', ParseMongoIdPipe) id: string): Promise<EntityResponse<StaffEntity>> {
      return this.orderProxy.send(patterns.staffCRUD, { meta: { params: { id }, CRUD: { method: 'delete' } } });
   }
}
