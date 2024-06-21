import { ApiEntityResponse, ApiPaginationResponse, HttpCache, Permission } from '@gateway/@library';
import { EntityResult, PaginationQueryDto, PaginationResult, SystemConfigDto } from '@shared-library';
import { ActivityLogDto } from '@microservice/system/dto';
import { serviceConfig } from '@metadata';
import { Body, Controller, Get, HttpStatus, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import { SystemConfigService } from '../provider';

const { permissions } = serviceConfig.get('system');

@ApiBearerAuth()
@ApiTags('Systems')
@Controller('systems')
export class SystemController {
   constructor(private readonly systemConfigService: SystemConfigService) {}

   @Post('config')
   @Permission({ key: permissions.config.save })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   saveConfig(@Body() data: SystemConfigDto): Promise<EntityResult<SystemConfigDto>> {
      return this.systemConfigService.saveConfig(data);
   }

   @Get('config')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.config.get })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   getConfig(): Promise<EntityResult<SystemConfigDto>> {
      return this.systemConfigService.getConfig();
   }

   @Get('activity-logs')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.activityLog.get })
   @ApiPaginationResponse(ActivityLogDto, { summary: 'Admin get list pagination of the activity logs' })
   activityLogsPaginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<ActivityLogDto>> {
      return this.systemConfigService.activityLogsPaginate(query);
   }
}
