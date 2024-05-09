import { Controller, HttpStatus, Inject, RequestMethod, UploadedFile } from '@nestjs/common';
import { ApiConsumes, ApiTags } from '@nestjs/swagger';
import { IData, IInterceptors, IRoute } from '@lib/decorator';
import { BaseController, ServiceExecuteResult } from '@lib';
import { FileProvider, FileService } from '../provider';
import { storageConfig } from '../storage.config';
import { FinalUploadDto, UploadDto } from '../dto';
import { FileEntity } from '../entity';
import { FileInterceptor } from '@nestjs/platform-express';

@ApiTags('Files')
@Controller('files')
export class FileController extends BaseController {
   @Inject(FileService) readonly fileService: FileService;
   @Inject(FileProvider) readonly fileProvider: FileProvider;

   @IInterceptors('gateway', FileInterceptor('file'))
   @ApiConsumes('multipart/form-data')
   @IRoute({
      pattern: storageConfig.patterns.upload,
      route: { method: RequestMethod.POST, httpStatus: HttpStatus.OK, path: 'upload' },
      swagger: { summary: 'Upload a file', responseType: FileEntity },
   })
   async upload(
      @IData() gatewayDto: UploadDto,
      @IData() serviceDto: FinalUploadDto,
      @UploadedFile() file: Express.Multer.File,
   ): ServiceExecuteResult<FileEntity> {
      let data: FinalUploadDto = serviceDto;

      await this.initGateway(async () => {
         data = await this.fileProvider.upload({ ...gatewayDto, file });
      });

      return await this.fileService.execute(storageConfig.patterns.upload, { data });
   }
}
