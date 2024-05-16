import { Injectable, NestMiddleware } from '@nestjs/common';
import { Response, NextFunction } from 'express';
import DeviceDetector from 'node-device-detector';
import DeviceHelper from 'node-device-detector/helper';
import { Registry } from '@mvanvu/ujs';
import { HttpRequest, RequestRegistryData } from '@lib/common/type';

@Injectable()
export class HttpMiddleware implements NestMiddleware {
   use(req: HttpRequest, _res: Response, next: NextFunction) {
      // Detect device
      const detector = new DeviceDetector();
      const userAgent = req.headers['user-agent'] || '';
      const result = detector.detect(userAgent);
      let device: RequestRegistryData['device'] = 'web';

      if (DeviceHelper.isMobileApp(result)) {
         device = 'mobile';
      } else if (DeviceHelper.isDesktopApp(result)) {
         device = 'desktop';
      }

      // Registry for local storage
      req.registry = Registry.from<RequestRegistryData>({
         device,
         userAgent,
         ipAddress: req.ips.length ? req.ips[0] : req.ip,
      });

      return next();
   }
}
