import { Module } from '@nestjs/common';
import { SystemController } from './controller';
import { ActivityLogProvider, FileProvider, MailerProvider, PurgeCacheProvider, SystemConfigService } from './provider';

@Module({
   controllers: [SystemController],
   providers: [ActivityLogProvider, PurgeCacheProvider, SystemConfigService, MailerProvider, FileProvider],
})
export class SystemModule {}
