import { Module } from '@nestjs/common';
import { FileService, PrismaService } from './provider';
import { FileController } from './controller';

@Module({
   controllers: [FileController],
   providers: [PrismaService, FileService],
})
export class StorageModule {}
