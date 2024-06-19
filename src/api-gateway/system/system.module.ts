import { Module } from '@nestjs/common';
import { SystemController } from './controller';
import { ActivityLogProvider, PurgeCacheProvider } from './provider';

@Module({
   controllers: [SystemController],
   providers: [ActivityLogProvider, PurgeCacheProvider],
})
export class SystemModule {}
