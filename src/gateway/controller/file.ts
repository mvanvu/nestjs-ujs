import { Body, Controller, HttpStatus, Inject, Post, UploadedFile, UseInterceptors } from '@nestjs/common';
import { ApiConsumes, ApiProperty, ApiResponse, ApiTags } from '@nestjs/swagger';
import { BaseController } from '../lib';
import { FileInterceptor } from '@nestjs/platform-express';
import { serviceConfig } from '@config';
import { FileProvider } from '../provider';
import { FileEntity, UploadDto } from '@lib/service/storage';
import { Permission } from '@lib';

@ApiTags('Files')
@Controller('files')
export class FileController extends BaseController {
   readonly storageProxy = this.createClientProxy(serviceConfig.get('storage.proxy'));

   @Inject(FileProvider) readonly fileProvider: FileProvider;

   @Post('upload')
   @Permission({ key: serviceConfig.get('storage.permissions.file.upload') })
   @UseInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @ApiProperty({ description: 'Upload a file' })
   @ApiResponse({ status: HttpStatus.OK, type: FileEntity })
   async upload(@Body() dto: UploadDto, @UploadedFile() file: Express.Multer.File): Promise<FileEntity> {
      const data = await this.fileProvider.upload({ ...dto, file });

      return await this.storageProxy.send(serviceConfig.get('storage.patterns.upload'), { data });
   }
}
