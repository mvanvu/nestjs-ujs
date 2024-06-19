import { Inject, Injectable } from '@nestjs/common';
import { SystemService } from '../provider';
import { serviceConfig } from '@metadata';
import { EventPattern, MessagePattern, Payload } from '@nestjs/microservices';
import { ActivityLogDto, SystemConfigDto, ActivityLogEntity } from '@lib/microservice/system';
import { CRUDResult } from '@lib/common';
const patterns = serviceConfig.get('system.patterns');

@Injectable()
export class SystemController {
   @Inject(SystemService) readonly systemService: SystemService;

   @MessagePattern(patterns.saveConfig)
   saveConfig(@Payload() dto: SystemConfigDto): Promise<SystemConfigDto> {
      return this.systemService.saveConfig(dto);
   }

   @MessagePattern(patterns.getConfig)
   getConfig(): Promise<SystemConfigDto> {
      return this.systemService.getConfig();
   }

   @EventPattern(patterns.writeActivityLog)
   writeActivityLog(@Payload() dto: ActivityLogDto): Promise<void> {
      return this.systemService.writeActivityLog(dto);
   }

   @MessagePattern(patterns.getActivityLog)
   executeCRUD(): Promise<CRUDResult<ActivityLogEntity>> {
      return this.systemService.executeCRUD();
   }
}
