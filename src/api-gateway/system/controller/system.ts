import { ApiEntityResponse, ApiPaginationResponse, BaseClientProxy, HttpCache, Permission } from '@gateway/@library';
import { EntityResult, PaginationQueryDto, PaginationResult, SystemConfigDto } from '@shared-library';
import { ActivityLogDto, FinalUploadDto, SendTestMailDto, UploadDto } from '@microservice/system/dto';
import { serviceConfig } from '@metadata';
import { Body, Controller, Get, HttpStatus, Inject, Post, Query, UploadedFile, UseInterceptors } from '@nestjs/common';
import { ApiBearerAuth, ApiConsumes, ApiTags } from '@nestjs/swagger';
import { FileProvider, MailerProvider, SystemConfigService } from '../provider';
import { FileEntity } from '@microservice/system/entity';
import { FileInterceptor } from '@nestjs/platform-express';

const { name, patterns, permissions } = serviceConfig.get('system');

@ApiBearerAuth()
@ApiTags('Systems')
@Controller()
export class SystemController {
   constructor(private readonly systemConfigService: SystemConfigService) {}

   @Inject(FileProvider) private readonly fileProvider: FileProvider;
   @Inject(MailerProvider) private readonly mailerProvider: MailerProvider;

   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get systemClient(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   @Post('systems/config')
   @Permission({ key: permissions.config.save, adminScope: true })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   saveConfig(@Body() data: SystemConfigDto): Promise<EntityResult<SystemConfigDto>> {
      return this.systemConfigService.saveConfig(data);
   }

   @Get('systems/config')
   @HttpCache({ disabled: true })
   @Permission({ key: permissions.config.get, adminScope: true })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   getConfig(): Promise<EntityResult<SystemConfigDto>> {
      return this.systemConfigService.getConfig();
   }

   @Get('systems/activity-logs')
   @Permission({ key: permissions.activityLog.get, adminScope: true })
   @ApiPaginationResponse(ActivityLogDto, { summary: 'Admin get list pagination of the activity logs' })
   activityLogsPaginate(@Query() query: PaginationQueryDto): Promise<PaginationResult<ActivityLogDto>> {
      return this.systemConfigService.activityLogsPaginate(query);
   }

   @Post('storages/upload')
   @Permission({ key: permissions.storage.upload })
   @UseInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @ApiEntityResponse(FileEntity, { summary: 'Upload a file' })
   async upload(@Body() dto: UploadDto, @UploadedFile() file: Express.Multer.File): Promise<EntityResult<FileEntity>> {
      const data = await this.fileProvider.upload({ ...dto, file });
      const fileEntity = await this.systemClient.send<EntityResult<FileEntity>, FinalUploadDto>(patterns.upload, data);
      fileEntity.data.url = this.fileProvider.getUrl(fileEntity.data);

      return fileEntity;
   }

   @Post('mails/send-test')
   @Permission({ key: permissions.mail.test, adminScope: true })
   @ApiEntityResponse(Boolean, { summary: 'Send a test email' })
   async sendTestMail(@Body() dto: SendTestMailDto): Promise<EntityResult<'OK' | 'FAILURE'>> {
      const result = await this.mailerProvider.sendTestEmail(dto);

      return { data: result.data === false ? 'FAILURE' : 'OK' };
   }
}
