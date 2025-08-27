import { Registry } from '@mvanvu/ujs';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { BaseEntity, SystemConfigDto, UserRefEntity } from '@shared-library';
import { ActivityLogDto, FinalUploadDto } from '../dto';
import { ActivityLogEntity, FileEntity } from '../entity';
import { BaseService, getSystemConfig, updateSystemConfig } from '@microservice/@library';

@Injectable()
export class SystemService extends BaseService {
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

      return updateSystemConfig(registry.valueOf());
   }

   async getConfig(): Promise<SystemConfigDto> {
      const systemConfig = getSystemConfig();

      if (systemConfig) {
         return systemConfig;
      }

      const config = await this.prisma.config.findUnique({ where: { key: this.systemKey }, select: { value: true } });

      return updateSystemConfig(config ? JSON.parse(config.value) : {});
   }

   async writeActivityLog(data: ActivityLogDto): Promise<void> {
      await this.prisma.activityLog.create({ data }).catch(console.debug);

      console.log(
         `Activity log created, message pattern: ${data.messagePattern}, success: ${data.success ? `true` : `false, error: ${JSON.stringify(data.dataResult.origin?.error ?? data.dataResult.origin)}`}`,
      );
   }

   createCRUDService() {
      return this.prisma.createCRUDService('activityLog', { entity: ActivityLogEntity, createDto: ActivityLogDto });
   }

   async upload(data: FinalUploadDto): Promise<FileEntity> {
      const user: UserRefEntity = this.meta.get('user');

      if (user) {
         Object.assign(data, { author: { id: user.id, username: user.username, email: user.email } });
      }

      return BaseEntity.bindToClass(await this.prisma.file.create({ data }), FileEntity);
   }
}
