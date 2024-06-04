import { Module } from '@nestjs/common';
import { EventEmitter } from '@mvanvu/ujs';
import { EventEmitterLoader } from './event-emitter.loader';
import { PurgeCacheProvider } from './purge-cache.provider';
import { DiscoveryModule } from '@nestjs/core';

@Module({
   imports: [DiscoveryModule],
   providers: [
      EventEmitterLoader,
      {
         provide: EventEmitter,
         useValue: new EventEmitter(),
      },
      PurgeCacheProvider,
   ],
   exports: [EventEmitter],
})
export class EventEmitterModule {}
