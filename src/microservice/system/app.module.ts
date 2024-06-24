import { Module } from '@nestjs/common';
import { createMicroserviceApp, getSystemConfig, updateSystemConfig } from '../@library';
import { serviceConfig } from '@metadata';
import { SystemController } from './controller';
import { SystemService, PrismaService, MailerService } from './provider';
import { Is } from '@mvanvu/ujs';
@Module({
   controllers: [SystemController],
   providers: [
      PrismaService,
      SystemService,
      {
         provide: MailerService,
         useFactory: async (systemService: SystemService) => {
            const systemConfig = getSystemConfig('system');

            if (Is.emptyObject(systemConfig)) {
               updateSystemConfig('system', await systemService.getConfig());
            }

            return MailerService;
         },
         inject: [SystemService],
      },
   ],
})
export class AppModule {
   static bootstrap(): Promise<void> {
      return createMicroserviceApp(AppModule, serviceConfig.get('system.name'));
   }
}
