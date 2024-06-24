import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDResult, SystemConfigDto } from '@shared-library';
import { ActivityLogDto } from '../dto';
import { ActivityLogEntity } from '../entity';

@Injectable()
export class SystemService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   private readonly systemKey: string = 'system';

   async saveConfig(dto: SystemConfigDto): Promise<SystemConfigDto> {
      const registry = Registry.from<SystemConfigDto>();
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

   async writeActivityLog(data: ActivityLogDto): Promise<void> {
      await this.prisma.activityLog.create({ data }).catch(console.debug);
      console.log(
         `Activity log created, message pattern: ${data.messagePattern}, success: ${data.success ? `true` : `false, error: ${JSON.stringify(data.dataResult.origin?.error ?? data.dataResult.origin)}`}`,
      );
   }

   executeCRUD(): Promise<CRUDResult<ActivityLogEntity>> {
      return this.prisma
         .createCRUDService('ActivityLog')
         .validateDTOPipe(ActivityLogDto)
         .entityResponse(ActivityLogEntity)
         .execute();
   }
}
