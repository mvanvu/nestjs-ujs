import { Controller, Inject } from '@nestjs/common';
import { MailerService, SystemService } from '../provider';
import { serviceConfig } from '@metadata';
import { EventPattern, MessagePattern, Payload } from '@nestjs/microservices';
import { CRUDResult, SystemConfigDto } from '@shared-library';
import { ActivityLogDto, FinalUploadDto, SendMailDto } from '../dto';
import { ActivityLogEntity, FileEntity, MessageInfoEntity } from '../entity';
const patterns = serviceConfig.get('system.patterns');

@Controller()
export class SystemController {
   @Inject(SystemService) readonly systemService: SystemService;
   @Inject(MailerService) readonly mailerService: MailerService;

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
      return this.systemService.createCRUDService().execute();
   }

   @EventPattern(patterns.sendMail)
   sendMail(dto: SendMailDto): Promise<MessageInfoEntity | false> {
      return this.mailerService.send(dto);
   }

   @MessagePattern(patterns.upload)
   upload(@Payload() data: FinalUploadDto): Promise<FileEntity> {
      return this.systemService.upload(data);
   }
}
