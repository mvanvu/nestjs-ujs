import { MessageData, MessageMeta, RequestRegistryData, FileEntity, FinalUploadDto } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { Registry } from '@mvanvu/ujs';
import { BaseService } from '@service/lib';

@Injectable()
export class FileService extends BaseService {
   @Inject(PrismaService) prisma: PrismaService;

   async upload({ data, meta }: MessageData<FinalUploadDto, Registry<MessageMeta>>): Promise<FileEntity> {
      const user: RequestRegistryData['user'] = meta?.get('headers.user');

      if (user) {
         Object.assign(data, { author: { id: user.id, username: user.username, email: user.email } });
      }

      return new FileEntity(await this.prisma.file.create({ data }));
   }
}
