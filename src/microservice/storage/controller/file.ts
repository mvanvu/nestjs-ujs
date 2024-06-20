import { Inject, Injectable } from '@nestjs/common';
import { FileService } from '../provider';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { serviceConfig } from '@metadata';
import { FinalUploadDto } from '../dto';
import { FileEntity } from '../entity';
const patterns = serviceConfig.get('storage.patterns');

@Injectable()
export class FileController {
   @Inject(FileService) readonly fileService: FileService;

   @MessagePattern(patterns.upload)
   upload(@Payload() data: FinalUploadDto): Promise<FileEntity> {
      return this.fileService.upload(data);
   }
}
