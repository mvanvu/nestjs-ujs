import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   BaseController,
   HttpCache,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto } from '@lib/common';
import { ActivityLogDto, SystemConfigDto } from '@lib/service/system';
import { serviceConfig } from '@metadata';
import { Body, Controller, Get, HttpStatus, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('system');

@ApiBearerAuth()
@ApiTags('Systems')
@Controller('systems')
export class SystemController extends BaseController {
   get systemProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Post('config')
   @Permission({ key: permissions.config.save })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   async saveConfig(@Body() data: SystemConfigDto): Promise<SystemConfigDto> {
      const configData = await this.systemProxy.send<SystemConfigDto, SystemConfigDto>(patterns.saveConfig, { data });
      await this.cacheManager.set(patterns.getConfig, configData);

      return configData;
   }

   @Get('config')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.config.get })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   async getConfig(): Promise<SystemConfigDto> {
      let configData: SystemConfigDto = await this.cacheManager.get(patterns.getConfig);

      if (!configData) {
         configData = await this.systemProxy.send<undefined, SystemConfigDto>(patterns.getConfig);
         await this.cacheManager.set(patterns.getConfig, configData);
      }

      return configData;
   }

   @Get('activity-logs')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.activityLog.get })
   @ApiPaginationResponse(ActivityLogDto, { summary: 'Admin get list pagination of the activity logs' })
   activityLogsPaginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<ActivityLogDto>> {
      return this.systemProxy.send(patterns.getActivityLog, { meta: { query, CRUD: { method: 'read' } } });
   }
}
