import { SystemConfigDto } from '@lib/service/system';
import { Registry } from '@mvanvu/ujs';
import { Injectable } from '@nestjs/common';
import * as fs from 'fs';

@Injectable()
export class SystemService {
   private readonly pathFile = `${process.cwd()}/src/system.config.json`;

   saveConfig(dto: SystemConfigDto): SystemConfigDto {
      const data: SystemConfigDto = fs.existsSync(this.pathFile)
         ? Registry.from(fs.readFileSync(this.pathFile).toString()).extends(dto).valueOf()
         : dto;

      fs.writeFileSync(this.pathFile, JSON.stringify(data, null, 2));

      return data;
   }

   getConfig(): SystemConfigDto {
      return fs.existsSync(this.pathFile) ? JSON.parse(fs.readFileSync(this.pathFile).toString()) : {};
   }
}
