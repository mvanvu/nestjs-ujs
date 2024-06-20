import { Body, Controller, Inject, Post, UploadedFile, UseInterceptors } from '@nestjs/common';
import { ApiConsumes, ApiTags, ApiBearerAuth } from '@nestjs/swagger';
import { ApiEntityResponse, BaseClientProxy, Permission } from '../../@library';
import { FileInterceptor } from '@nestjs/platform-express';
import { FileProvider } from '../provider/file.provider';
import { serviceConfig } from '@metadata';
import { FileEntity } from '@microservice/storage/entity';
import { UploadDto } from '@microservice/storage/dto';

const { name, permissions, patterns } = serviceConfig.get('storage');

@ApiBearerAuth()
@ApiTags('Storages')
@Controller('storages')
export class StorageController {
   @Inject(FileProvider) private readonly fileProvider: FileProvider;

   @Inject(BaseClientProxy) private readonly proxy: BaseClientProxy;

   get storageClient(): BaseClientProxy {
      return this.proxy.createClient(name);
   }

   @Post('upload')
   @Permission({ key: permissions.upload })
   @UseInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @ApiEntityResponse(FileEntity, { summary: 'Upload a file' })
   async upload(@Body() dto: UploadDto, @UploadedFile() file: Express.Multer.File): Promise<FileEntity> {
      const data = await this.fileProvider.upload({ ...dto, file });

      return await this.storageClient.send(patterns.upload, { data });
   }
}
