import { Inject, Injectable } from '@nestjs/common';
import { SystemService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { SystemConfigDto } from '@lib/service/system';
const patterns = serviceConfig.get('system.patterns');

@Injectable()
export class SystemController {
   @Inject(SystemService) readonly systemService: SystemService;

   @MessagePattern(patterns.saveConfig)
   saveConfig(@Payload() dto: SystemConfigDto): SystemConfigDto {
      return this.systemService.saveConfig(dto);
   }

   @MessagePattern(patterns.getConfig)
   getConfig(): SystemConfigDto {
      return this.systemService.getConfig();
   }
}
