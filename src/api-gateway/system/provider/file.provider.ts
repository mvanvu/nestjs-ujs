import { Injectable, StreamableFile } from '@nestjs/common';
import { FileProviderLocal } from './file.provider.local';
import { FileProviderGoogleDrive } from './file.provider.google-drive';
import { StorageProvider } from '.prisma/system';
import { FileProviderInterface } from './file.provider.interface';
import { Transform } from '@mvanvu/ujs';
import { serviceConfig } from '@metadata';
import { ThrowException } from '@shared-library';
import { allAcceptedFileTypes, fileTypes } from './file.mime';
import { FinalUploadDto, UploadDto } from '@microservice/system/dto';
import { FileEntity } from '@microservice/system/entity';
const storageConfig = serviceConfig.get('system');

@Injectable()
export class FileProvider {
   getStorageDriver(provider?: StorageProvider): FileProviderInterface {
      provider = provider ?? storageConfig.upload.provider;

      switch (provider) {
         case StorageProvider.GoogleDrive:
            return new FileProviderGoogleDrive();

         case StorageProvider.Local:
            return new FileProviderLocal();

         default:
            ThrowException(`The media storage must be in (${Object.values(StorageProvider)})`);
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
