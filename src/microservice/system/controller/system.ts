import { Controller, Inject } from '@nestjs/common';
import { SystemService } from '../provider';
import { serviceConfig } from '@metadata';
import { EventPattern, MessagePattern, Payload } from '@nestjs/microservices';
import { CRUDResult, SystemConfigDto } from '@shared-library';
import { ActivityLogDto } from '../dto';
import { ActivityLogEntity } from '../entity';
const patterns = serviceConfig.get('system.patterns');

@Controller()
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
