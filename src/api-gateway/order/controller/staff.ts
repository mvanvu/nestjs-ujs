import { ApiEntityResponse, ApiPaginationResponse, BaseClientProxy } from '@gateway/@library';
import { CRUDClient, EntityResult, PaginationQueryDto, PaginationResult, ParseTypePipe } from '@shared-library';
import { serviceConfig } from '@metadata';
import { Body, Controller, Delete, Get, HttpStatus, Inject, Param, Patch, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { StaffEntity } from '@microservice/order/entity';
import { CreateStaffDto, UpdateStaffDto } from '@microservice/order/dto';

const { name, patterns } = serviceConfig.get('order');

@ApiBearerAuth()
@ApiTags('Orders')
@Controller('order/staffs')
export class OrderStaffController {
   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get staffCRUD(): CRUDClient {
      return this.proxy.createClient(name).createCRUD(patterns.staffCRUD);
   }

   @ApiPaginationResponse(StaffEntity, { summary: 'Get list pagination of restaurant staffs' })
   @Get()
   paginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<StaffEntity>> {
      return this.staffCRUD.paginate(query);
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Get detail of the staff' })
   @Get(':id')
   read(@Param('id', ParseTypePipe('mongoId')) id: string): Promise<EntityResult<StaffEntity>> {
      return this.staffCRUD.read(id);
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Create a new staff', statusCode: HttpStatus.CREATED })
   @Post()
   create(@Body() data: CreateStaffDto): Promise<EntityResult<StaffEntity>> {
      return this.staffCRUD.create(data);
   }

   @Patch(':id')
   @ApiEntityResponse(StaffEntity, { summary: 'Update a staff' })
   update(
      @Param('id', ParseTypePipe('mongoId')) id: string,
      @Body() data: UpdateStaffDto,
   ): Promise<EntityResult<StaffEntity>> {
      return this.staffCRUD.update(id, data);
   }

   @ApiEntityResponse(StaffEntity, { summary: 'Delete a table' })
   @Delete(':id')
   delete(@Param('id', ParseTypePipe('mongoId')) id: string): Promise<EntityResult<StaffEntity>> {
      return this.staffCRUD.delete(id);
   }
}
