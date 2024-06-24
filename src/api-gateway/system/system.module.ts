import { Module } from '@nestjs/common';
import { SystemController } from './controller';
import { ActivityLogProvider, MailerProvider, PurgeCacheProvider, SystemConfigService } from './provider';

@Module({
   controllers: [SystemController],
   providers: [ActivityLogProvider, PurgeCacheProvider, SystemConfigService, MailerProvider],
})
export class SystemModule {}
