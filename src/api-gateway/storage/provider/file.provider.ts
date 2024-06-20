import { Injectable, StreamableFile } from '@nestjs/common';
import { FileProviderLocal } from './file.provider.local';
import { FileProviderGoogleDrive } from './file.provider.google-drive';
import { Provider } from '.prisma/storage';
import { FileProviderInterface } from './file.provider.interface';
import { Transform } from '@mvanvu/ujs';
import { serviceConfig } from '@metadata';
import { ThrowException } from '@shared-library';
import { allAcceptedFileTypes, fileTypes } from './file.mime';
import { FinalUploadDto, UploadDto } from '@microservice/storage/dto';
import { FileEntity } from '@microservice/storage/entity';
const storageConfig = serviceConfig.get('storage');

@Injectable()
export class FileProvider {
   getStorageDriver(provider?: Provider): FileProviderInterface {
      provider = provider ?? storageConfig.upload.provider;

      switch (provider) {
         case Provider.GoogleDrive:
            return new FileProviderGoogleDrive();

         case Provider.Local:
            return new FileProviderLocal();

         default:
            ThrowException(`The media storage must be in (${Object.values(Provider)})`);
      }
   }

   upload(dto: UploadDto): Promise<FinalUploadDto> {
      // Validate file first
      const { file, fileType } = dto;

      switch (fileType) {
         case 'Image':
            if (!fileTypes.image.includes(file.mimetype)) {
               ThrowException('File is not an image');
            }

            break;

         case 'Video':
            if (!fileTypes.video.includes(file.mimetype)) {
               ThrowException('File is not a video');
            }

            break;

         case 'Media':
            if (!fileTypes.media.includes(file.mimetype)) {
               ThrowException('File is not a media');
            }

            break;

         case 'Document':
            if (!fileTypes.document.includes(file.mimetype)) {
               ThrowException('File is not a document');
            }

            break;

         case 'Unknown':
         default:
            if (!allAcceptedFileTypes.includes(file.mimetype)) {
               ThrowException('The type of file is not allow');
            }

            break;
      }

      dto.file.filename = Transform.toSafeFileName(dto.file.originalname);

      return this.getStorageDriver().upload(dto);
   }

   getUrl(file: FileEntity): string {
      return this.getStorageDriver(file.provider).getUrl(file);
   }

   stream(file: FileEntity): Promise<StreamableFile> {
      return this.getStorageDriver(file.provider).stream(file);
   }
}
