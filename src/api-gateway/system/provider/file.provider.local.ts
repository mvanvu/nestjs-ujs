import { BadRequestException, StreamableFile } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';
import { randomUUID } from 'crypto';
import { FileProviderInterface } from './file.provider.interface';
import { serviceConfig } from '@metadata';
import { FinalUploadDto, UploadDto } from '@microservice/system/dto';
import { FileEntity } from '@microservice/system/entity';
const storageConfig = serviceConfig.get('system');

export class FileProviderLocal implements FileProviderInterface {
   private readonly storePath: string;
   private readonly storeUrl: string;

   constructor() {
      const basePath = storageConfig.upload.localPath;

      if (!basePath) {
         throw new Error('ENV not provided: MEDIA_STORAGE_LOCAL_PATH');
      }

      this.storePath = path.join(process.cwd(), basePath);
      this.storeUrl = `/${storageConfig.upload.prefix}`;

      if (!fs.existsSync(this.storePath)) {
         fs.mkdirSync(`${this.storePath}/public`, { recursive: true });
         fs.mkdirSync(`${this.storePath}/secret`, { recursive: true });
      }
   }

   async upload(dto: UploadDto): Promise<FinalUploadDto | null> {
      try {
         const providerId = randomUUID();
         fs.writeFileSync(
            `${this.storePath}/${dto.isPublic ? 'public' : 'secret'}/${providerId}-${dto.file.filename}`,
            dto.file.buffer,
         );

         return {
            name: dto.file.filename,
            providerId,
            mime: dto.file.mimetype,
            size: dto.file.size,
            provider: 'Local',
            isPublic: dto.isPublic,
            type: dto.fileType,
         };
      } catch (error) {
         console.debug('Error uploading file to local storage:', error);
         return null;
      }
   }

   getUrl(file: FileEntity): string {
      if (file.isPublic) {
         return `${this.storeUrl}/${file.providerId}-${file.name}`;
      }

      return `/${this.storeUrl}/${file.id}/stream`;
   }

   async stream(file: FileEntity): Promise<StreamableFile> {
      if (file.provider !== 'Local') {
         throw new BadRequestException(`The storage of file[${file.name}] is not ${file.provider}`);
      }

      const filePath = `${this.storePath}/${file.isPublic ? 'public' : 'secret'}/${file.providerId}-${file.name}`;

      try {
         return new StreamableFile(fs.createReadStream(filePath), {
            type: file.mime,
            disposition: `attachment; filename="${file.name.replace(/"/g, '')}"`,
         });
      } catch (error) {
         console.debug('Error fetching file from local storage:', error);
      }
   }
}
