import { Inject, Injectable } from '@nestjs/common';
import { FileService } from '../provider';
import { MessagePattern, Payload } from '@nestjs/microservices';
import { FileEntity, FinalUploadDto } from '@lib/service';
import { serviceConfig } from '@config';
const patterns = serviceConfig.get('storage.patterns');

@Injectable()
export class FileController {
   @Inject(FileService) readonly fileService: FileService;

   @MessagePattern(patterns.upload)
   upload(@Payload() data: FinalUploadDto): Promise<FileEntity> {
      return this.fileService.upload(data);
   }
}
