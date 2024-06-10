import { ActivityLogDto, SystemConfigDto } from '@lib/service/system';
import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CRUDResult } from '@lib/common';
import { ActivityLogEntity } from '@lib/service/system/entity';

@Injectable()
export class SystemService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   private readonly systemKey: string = 'system';

   async saveConfig(dto: SystemConfigDto): Promise<SystemConfigDto> {
      const registry = Registry.from<SystemConfigDto>({});
      const prevData = await this.prisma.config.findUnique({ where: { key: this.systemKey }, select: { value: true } });

      if (prevData) {
         registry.extends(prevData.value);
      }

      const value = registry.extends(dto).toString();
      await this.prisma.config.upsert({
         where: { key: this.systemKey },
         create: { key: this.systemKey, value },
         update: { value },
      });

      return registry.valueOf();
   }

   async getConfig(): Promise<SystemConfigDto> {
      const config = await this.prisma.config.findUnique({ where: { key: this.systemKey }, select: { value: true } });

      return (config ? JSON.parse(config.value) : {}) as SystemConfigDto;
   }

   writeActivityLog(data: ActivityLogDto): void {
      this.prisma.activityLog.create({ data });
   }

   executeCRUD(): Promise<CRUDResult<ActivityLogEntity>> {
      return this.prisma
         .createCRUDService('ActivityLog')
         .validateDTOPipe(ActivityLogDto)
         .entityResponse(ActivityLogEntity)
         .execute();
   }
}
