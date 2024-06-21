import { BaseClientProxy } from '@gateway/@library';
import { serviceConfig } from '@metadata';
import { ActivityLogDto } from '@microservice/system/dto';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { Inject, Injectable } from '@nestjs/common';
import { EntityResult, PaginationQueryDto, PaginationResult, SystemConfigDto } from '@shared-library';

const { name, patterns } = serviceConfig.get('system');

@Injectable()
export class SystemConfigService {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get clientProxy(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   async getConfig(): Promise<EntityResult<SystemConfigDto>> {
      let configData: EntityResult<SystemConfigDto> = await this.cacheManager.get(patterns.getConfig);

      if (!configData) {
         configData = await this.clientProxy.send<EntityResult<SystemConfigDto>, SystemConfigDto>(patterns.getConfig);
         await this.cacheManager.set(patterns.getConfig, configData, 0);
      }

      return configData;
   }

   async saveConfig(data: SystemConfigDto): Promise<EntityResult<SystemConfigDto>> {
      const configData = await this.clientProxy.send<EntityResult<SystemConfigDto>, SystemConfigDto>(
         patterns.saveConfig,
         data,
      );

      // Store the config into cache store
      await this.cacheManager.set(patterns.getConfig, configData, 0);

      return configData;
   }

   activityLogsPaginate(query: PaginationQueryDto): Promise<PaginationResult<ActivityLogDto>> {
      return this.clientProxy.createCRUD(patterns.getActivityLog).paginate(query);
   }
}
