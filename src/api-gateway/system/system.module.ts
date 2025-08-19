import { Module } from '@nestjs/common';
import { SystemController } from './controller';
import { ActivityLogProvider, FileProvider, MailerProvider, PurgeCacheProvider, SystemConfigService } from './provider';
import { CacheService } from '@shared-library';

@Module({
   controllers: [SystemController],
   providers: [
      ActivityLogProvider,
      PurgeCacheProvider,
      SystemConfigService,
      MailerProvider,
      FileProvider,
      CacheService,
   ],
})
export class SystemModule {}
