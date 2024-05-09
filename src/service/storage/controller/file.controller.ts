import { Controller, HttpStatus, Inject, RequestMethod, UploadedFile } from '@nestjs/common';
import { ApiConsumes, ApiTags } from '@nestjs/swagger';
import { IGatewayInterceptors, IGatewayPayload, IRoute, IServicePayload } from '@lib/decorator';
import { ServiceExecuteResult, metadata } from '@lib';
import { FileProviderLocal, FileService } from '../provider';
import { storageConfig } from '../storage.config';
import { FinalUploadDto, UploadDto } from '../dto';
import { FileEntity } from '../entity';
import { FileInterceptor } from '@nestjs/platform-express';

@ApiTags('Files')
@Controller('files')
export class FileController {
   @Inject(FileService) readonly fileService: FileService;

   @IGatewayInterceptors(FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @IRoute({
      pattern: storageConfig.patterns.upload,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'upload' },
      swagger: { summary: 'Upload a file', responseType: FileEntity },
   })
   async upload(
      @IGatewayPayload() gatewayDto: UploadDto,
      @IServicePayload() serviceDto: FinalUploadDto,
      @UploadedFile() file: Express.Multer.File,
   ): ServiceExecuteResult<FileEntity> {
      let data: FinalUploadDto = serviceDto;

      if (metadata.isGateway()) {
         data = await new FileProviderLocal().upload({ ...gatewayDto, file });
      }

      return await this.fileService.execute(storageConfig.patterns.upload, { data });
   }
}
