import { BadRequestException, StreamableFile } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';
import { randomUUID } from 'crypto';
import { FileProviderInterface } from './file.provider.interface';
import { appConfig } from '@lib';
import { UploadDto, FinalUploadDto } from '../dto';
import { Transform, Util } from '@mvanvu/ujs';
import { FileEntity } from '../entity';

export class FileProviderLocal implements FileProviderInterface {
   private readonly storePath: string;
   private readonly storeUrl: string;

   constructor() {
      const basePath = appConfig.storage.localPath;

      if (!basePath) {
         throw new Error('ENV not provided: MEDIA_STORAGE_LOCAL_PATH');
      }

      this.storePath = path.join(process.cwd(), basePath);
      this.storeUrl = `/${appConfig.storage.prefix}`;

      if (!fs.existsSync(this.storePath)) {
         fs.mkdirSync(`${this.storePath}/public`, { recursive: true });
         fs.mkdirSync(`${this.storePath}/secret`, { recursive: true });
      }
   }

   async upload(dto: UploadDto): Promise<FinalUploadDto | null> {
      try {
         const providerId = randomUUID();
         const name = Transform.toSafeFileName(dto.file.originalname);
         fs.writeFileSync(
            `${this.storePath}/${dto.isPublic ? 'public' : 'secret'}/${providerId}-${name}`,
            dto.file.buffer,
         );

         return {
            name,
            providerId,
            mime: dto.file.mimetype,
            size: dto.file.size,
            provider: 'Local',
            isPublic: dto.isPublic,
            type: dto.fileType,
         };
      } catch (error) {
         Util.debug('Error uploading file to local storage:', error);
         return null;
      }
   }

   getUrl(file: FileEntity) {
      if (file.isPublic) {
         return `${this.storeUrl}/${file.providerId}-${file.name}`;
      }

      return `/${this.storeUrl}/${file.id}/stream`;
   }

   async stream(file: FileEntity) {
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
         Util.debug('Error fetching file from local storage:', error);
      }
   }
}
