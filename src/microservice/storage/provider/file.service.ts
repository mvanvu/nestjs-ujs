import { RequestRegistryData, FileEntity, FinalUploadDto } from '@lib';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { BaseService } from '@service/lib';

@Injectable()
export class FileService extends BaseService {
   @Inject(PrismaService) prisma: PrismaService;

   async upload(data: FinalUploadDto): Promise<FileEntity> {
      const user: RequestRegistryData['user'] = this.meta?.get('headers.user');

      if (user) {
         Object.assign(data, { author: { id: user.id, username: user.username, email: user.email } });
      }

      return new FileEntity(await this.prisma.file.create({ data }));
   }
}
