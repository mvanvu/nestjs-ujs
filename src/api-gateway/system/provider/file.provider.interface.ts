import { FinalUploadDto, UploadDto } from '@microservice/system/dto';
import { FileEntity } from '@microservice/system/entity';
import { StreamableFile } from '@nestjs/common';

export interface FileProviderInterface {
   upload(dto: UploadDto): Promise<FinalUploadDto | null>;
   getUrl(file: FileEntity): string;
   stream(file: FileEntity): Promise<StreamableFile>;
}
