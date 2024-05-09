import { BaseService, MessageData, MessageMeta, RequestRegistryData, ServiceOptions } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { storageConfig } from '../storage.config';
import { PrismaService } from './prisma/prisma.service';
import { FinalUploadDto } from '../dto';
import { FileEntity } from '../entity';
import { Registry } from '@mvanvu/ujs';

@Injectable()
export class FileService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   readonly options: ServiceOptions = { config: storageConfig };

   async upload({ data, meta }: MessageData<FinalUploadDto, Registry<MessageMeta>>): Promise<FileEntity> {
      const user: RequestRegistryData['user'] = meta?.get('headers.user');

      if (user) {
         Object.assign(data, { author: { id: user.id, username: user.username, email: user.email } });
      }

      return new FileEntity(await this.prisma.file.create({ data }));
   }
}
