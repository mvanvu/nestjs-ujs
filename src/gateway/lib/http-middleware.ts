import { Inject, Injectable, NestMiddleware } from '@nestjs/common';
import { Response, NextFunction } from 'express';
import DeviceDetector from 'node-device-detector';
import DeviceHelper from 'node-device-detector/helper';
import { Registry } from '@mvanvu/ujs';
import { HttpRequest, RequestRegistryData } from '@lib/common';
import { CACHE_MANAGER, Cache } from '@nestjs/cache-manager';
import { SystemConfigDto } from '@lib/service/system';
import { serviceConfig } from '@metadata';
import { ClientProxy } from '@nestjs/microservices';
import { lastValueFrom } from 'rxjs';

@Injectable()
export class HttpMiddleware implements NestMiddleware {
   @Inject(CACHE_MANAGER) private readonly cacheManager: Cache;

   @Inject(serviceConfig.get('system.name').toUpperCase() + '_MICROSERVICE')
   private readonly systemProxy: ClientProxy;

   async use(req: HttpRequest, _res: Response, next: NextFunction) {
      // Detect device
      const detector = new DeviceDetector();
      const userAgent = req.headers['user-agent'] || '';
      const result = detector.detect(userAgent);
      let deviceType: RequestRegistryData['deviceType'] = 'web';

      if (DeviceHelper.isMobileApp(result)) {
         deviceType = 'mobile';
      } else if (DeviceHelper.isDesktopApp(result)) {
         deviceType = 'desktop';
      }

      // Load system config
      const systemConfigPattern = serviceConfig.get('system.patterns.getConfig');
      let systemConfig: SystemConfigDto = await this.cacheManager.get(systemConfigPattern);

      if (!systemConfig) {
         systemConfig = await lastValueFrom<SystemConfigDto>(this.systemProxy.send(systemConfigPattern, {}).pipe());
         await this.cacheManager.set(systemConfigPattern, systemConfig, 0);
      }

      // Registry for local storage
      req.registry = Registry.from<RequestRegistryData>({
         deviceOS: result.os
            ? {
                 name: result.os.name,
                 shortName: result.os.short_name,
                 platform: result.os.platform,
                 version: result.os.version,
              }
            : null,
         deviceType,
         userAgent,
         ipAddress: req.ips.length ? req.ips[0] : req.ip,
         systemConfig,
      });

      return next();
   }
}
