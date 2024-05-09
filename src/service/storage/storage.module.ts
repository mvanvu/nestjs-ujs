import { Module } from '@nestjs/common';
import { FileProvider, FileService, PrismaService } from './provider';
import { FileController } from './controller';

@Module({
   controllers: [FileController],
   providers: [PrismaService, FileService, FileProvider],
})
export class StorageModule {}
