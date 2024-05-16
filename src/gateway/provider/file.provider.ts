import { Injectable, StreamableFile } from '@nestjs/common';
import { FileEntity, FinalUploadDto, UploadDto } from '@lib/service';
import { FileProviderLocal } from './file.provider.local';
import { FileProviderGoogleDrive } from './file.provider.google-drive';
import { Provider } from '.prisma/storage';
import { FileProviderInterface } from './file.provider.interface';
import { Transform } from '@mvanvu/ujs';
import { appConfig } from '@config';
import { ThrowException } from '@lib/common';
@Injectable()
export class FileProvider {
   getStorageDriver(provider?: Provider): FileProviderInterface {
      provider = provider ?? <Provider>appConfig.get('storage.provider');

      switch (provider) {
         case Provider.GoogleDrive:
            return new FileProviderGoogleDrive();

         case Provider.Local:
            return new FileProviderLocal();

         default:
            throw new ThrowException(`The media storage must be in (${Object.values(Provider)})`);
      }
   }

   upload(dto: UploadDto): Promise<FinalUploadDto> {
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
