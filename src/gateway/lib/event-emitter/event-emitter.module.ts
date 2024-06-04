import { Module } from '@nestjs/common';
import { EventEmitterLoader } from './event-emitter.loader';
import { EventEmitter } from '@mvanvu/ujs';

@Module({
   providers: [
      EventEmitterLoader,
      {
         provide: EventEmitter,
         useValue: new EventEmitter(),
      },
   ],
})
export class EventEmitterModule {}
