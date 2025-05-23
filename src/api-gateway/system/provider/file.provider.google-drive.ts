import { ThrowException } from '@shared-library';
import { FileProviderInterface } from './file.provider.interface';
import { google, drive_v3 as v3 } from 'googleapis';
import { Readable } from 'stream';
import { StorageProvider } from '.prisma/system';
import { StreamableFile } from '@nestjs/common';
import { serviceConfig } from '@metadata';
import { FinalUploadDto, UploadDto } from '@microservice/system/dto';
import { FileEntity } from '@microservice/system/entity';

const storageConfig = serviceConfig.get('system');

export class FileProviderGoogleDrive implements FileProviderInterface {
   private readonly storage: v3.Drive;

   constructor() {
      this.storage = google.drive({
         version: 'v3',
         auth: new google.auth.GoogleAuth({
            keyFile: `${process.cwd()}/${storageConfig.upload.googleDriveCredentialsPath}`,
            scopes: ['https://www.googleapis.com/auth/drive'],
         }),
      });
   }

   async upload(dto: UploadDto): Promise<FinalUploadDto | null> {
      try {
         const fileMetadata: v3.Schema$File = {
            name: dto.file.filename,
            mimeType: dto.file.mimetype,
         };
         const media: v3.Params$Resource$Files$Create['media'] = {
            mimeType: 'application/octet-stream',
            body: Readable.from(dto.file.buffer),
         };

         const response = await this.storage.files.create({ requestBody: fileMetadata, media, fields: 'id' });

         if (dto.isPublic) {
            await this.storage.permissions.create({
               fileId: response.data.id,
               requestBody: { role: 'reader', type: 'anyone' },
            });
         }

         return {
            type: dto.fileType,
            name: fileMetadata.name,
            mime: fileMetadata.mimeType,
            size: dto.file.size,
            provider: StorageProvider.GoogleDrive,
            providerId: response.data.id,
            isPublic: dto.isPublic,
         };
      } catch (error) {
         console.debug('Error uploading file to Google Drive:', error);

         return null;
      }
   }

   getUrl(file: FileEntity): string {
      if (file.isPublic) {
         return `https://drive.google.com/uc?id=${file.providerId}&export=download`;
      }

      return `/${storageConfig.upload.prefix}/files/${file.id}/stream`;
   }

   async stream(file: FileEntity): Promise<StreamableFile> {
      if (file.provider !== StorageProvider.GoogleDrive) {
         throw new ThrowException(`The provider of file[${file.name}] is ${file.provider} not Google Drive`);
      }

      try {
         const { data } = await this.storage.files.get(
            { fileId: file.providerId, alt: 'media' }, // alt: 'media' -> is important
            { responseType: 'stream' },
         );

         return new StreamableFile(data, { type: file.mime, disposition: `attachment; filename="${file.name}"` });
      } catch (error) {
         console.error('Error fetching file from Google Drive:', error);
      }
   }
}
