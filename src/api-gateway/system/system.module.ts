import { Module } from '@nestjs/common';
import { SystemController } from './controller';
import { ActivityLogProvider, PurgeCacheProvider, SystemConfigService } from './provider';

@Module({
   controllers: [SystemController],
   providers: [ActivityLogProvider, PurgeCacheProvider, SystemConfigService],
})
export class SystemModule {}
