import { Module } from '@nestjs/common';
import { StorageController } from './controller';
import { FileProvider } from './provider';

@Module({
   controllers: [StorageController],
   providers: [FileProvider],
})
export class StorageModule {}
