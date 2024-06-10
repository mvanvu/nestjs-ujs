import { ApiEntityResponse, BaseController, Permission } from '@gateway/lib';
import { SystemConfigDto } from '@lib/service/system';
import { serviceConfig } from '@metadata';
import { Body, Controller, HttpStatus, Post } from '@nestjs/common';
import { ApiBearerAuth, ApiTags } from '@nestjs/swagger';
import * as fs from 'fs';

const { permissions } = serviceConfig.get('system');

@ApiBearerAuth()
@ApiTags('systems')
@Controller('systems')
export class SystemController extends BaseController {
   @Post('config')
   @Permission({ key: permissions.config.save })
   @ApiEntityResponse(SystemConfigDto, { statusCode: HttpStatus.OK })
   config(@Body() dto: SystemConfigDto): SystemConfigDto {
      fs.writeFileSync(`${process.cwd()}/src/system.config.json`, JSON.stringify(dto, null, 2));

      return new SystemConfigDto(dto);
   }
}
