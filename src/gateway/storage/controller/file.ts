import { Body, Controller, Inject, Post, UploadedFile, UseInterceptors } from '@nestjs/common';
import { ApiConsumes, ApiTags, ApiBearerAuth } from '@nestjs/swagger';
import { ApiEntityResponse, BaseClientProxy, BaseController, Permission } from '../../lib';
import { FileInterceptor } from '@nestjs/platform-express';
import { FileProvider } from '../provider/file.provider';
import { FileEntity, UploadDto } from '@lib/service/storage';
import { serviceConfig } from '@metadata';

const { name, permissions, patterns } = serviceConfig.get('storage');

@ApiBearerAuth()
@ApiTags('Files')
@Controller('files')
export class FileController extends BaseController {
   @Inject(FileProvider) readonly fileProvider: FileProvider;

   get storageProxy(): BaseClientProxy {
      return this.createClientProxy(name);
   }

   @Post('upload')
   @Permission({ key: permissions.file.upload })
   @UseInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @ApiEntityResponse(FileEntity, { summary: 'Upload a file' })
   async upload(@Body() dto: UploadDto, @UploadedFile() file: Express.Multer.File): Promise<FileEntity> {
      const data = await this.fileProvider.upload({ ...dto, file });

      return await this.storageProxy.send(patterns.upload, { data });
   }
}
