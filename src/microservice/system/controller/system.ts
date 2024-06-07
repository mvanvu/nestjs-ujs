import { Inject, Injectable } from '@nestjs/common';
import { SystemService } from '../provider';
import { serviceConfig } from '@metadata';
const patterns = serviceConfig.get('system.patterns');

@Injectable()
export class SystemController {
   @Inject(SystemService) readonly systemService: SystemService;
}
