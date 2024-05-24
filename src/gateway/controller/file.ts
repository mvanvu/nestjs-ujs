import { Body, Controller, Inject, Post, UploadedFile, UseInterceptors } from '@nestjs/common';
import { ApiConsumes, ApiTags } from '@nestjs/swagger';
import { BaseClientProxy, BaseController } from '../lib';
import { FileInterceptor } from '@nestjs/platform-express';
import { serviceConfig } from '@lib/service';
import { FileProvider } from '../provider';
import { FileEntity, UploadDto } from '@lib/service/storage';
import { ApiResultResponse, Permission } from '@lib/common';

@ApiTags('Files')
@Controller('files')
export class FileController extends BaseController {
   @Inject(FileProvider) readonly fileProvider: FileProvider;

   get storageProxy(): BaseClientProxy {
      return this.createClientProxy(serviceConfig.get('storage.name'));
   }

   @Post('upload')
   @Permission({ key: serviceConfig.get('storage.permissions.file.upload') })
   @UseInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @ApiResultResponse(() => FileEntity, { summary: 'Upload a file' })
   async upload(@Body() dto: UploadDto, @UploadedFile() file: Express.Multer.File): Promise<FileEntity> {
      const data = await this.fileProvider.upload({ ...dto, file });

      return await this.storageProxy.send(serviceConfig.get('storage.patterns.upload'), { data });
   }
}
