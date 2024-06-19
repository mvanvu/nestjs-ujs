import {
   ApiEntityResponse,
   ApiPaginationResponse,
   BaseClientProxy,
   HttpCache,
   PaginationResponse,
   Permission,
} from '@gateway/lib';
import { PaginationQueryDto } from '@lib/common';
import { ActivityLogDto, SystemConfigDto } from '@lib/microservice/system';
import { serviceConfig } from '@metadata';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { Body, Controller, Get, HttpStatus, Inject, Post, Query } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';

const { name, permissions, patterns } = serviceConfig.get('system');

@ApiBearerAuth()
@ApiTags('Systems')
@Controller('systems')
export class SystemController {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get systemClient(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   @Post('config')
   @Permission({ key: permissions.config.save })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   async saveConfig(@Body() data: SystemConfigDto): Promise<SystemConfigDto> {
      const configData = await this.systemClient.send<SystemConfigDto, SystemConfigDto>(patterns.saveConfig, { data });
      await this.cacheManager.set(patterns.getConfig, configData, 0);

      return configData;
   }

   @Get('config')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.config.get })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   async getConfig(): Promise<SystemConfigDto> {
      let configData: SystemConfigDto = await this.cacheManager.get(patterns.getConfig);

      if (!configData) {
         configData = await this.systemClient.send<undefined, SystemConfigDto>(patterns.getConfig);
         await this.cacheManager.set(patterns.getConfig, configData, 0);
      }

      return configData;
   }

   @Get('activity-logs')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.activityLog.get })
   @ApiPaginationResponse(ActivityLogDto, { summary: 'Admin get list pagination of the activity logs' })
   activityLogsPaginate(@Query() query: PaginationQueryDto): Promise<PaginationResponse<ActivityLogDto>> {
      return this.systemClient.createCRUD(patterns.getActivityLog).paginate(query);
   }
}
