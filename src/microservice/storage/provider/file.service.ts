import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { BaseService } from '@microservice/@library';
import { BaseEntity, UserRefEntity } from '@shared-library';
import { FinalUploadDto } from '../dto';
import { FileEntity } from '../entity';

@Injectable()
export class FileService extends BaseService {
   @Inject(PrismaService) prisma: PrismaService;

   async upload(data: FinalUploadDto): Promise<FileEntity> {
      const user: UserRefEntity = this.meta?.get('user');

      if (user) {
         Object.assign(data, { author: { id: user.id, username: user.username, email: user.email } });
      }

      return BaseEntity.bindToClass(await this.prisma.file.create({ data }), FileEntity);
   }
}
