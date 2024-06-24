import { ServiceName } from '@metadata';
import { ObjectRecord } from '@mvanvu/ujs';
import * as fs from 'fs';

const dataConfig: ObjectRecord = {};
const getSystemConfigPath = (serviceName: ServiceName) =>
   `${process.cwd()}/src/microservice/${serviceName}/system.config.json`;

export const getSystemConfig = <T>(serviceName: ServiceName): T => {
   const systemConfigPath = getSystemConfigPath(serviceName);

   if (!dataConfig[serviceName]) {
      dataConfig[serviceName] = fs.existsSync(systemConfigPath)
         ? JSON.parse(fs.readFileSync(systemConfigPath).toString())
         : {};
   }

   return dataConfig;
};

export const updateSystemConfig = (serviceName: ServiceName, serviceConfigData: ObjectRecord): void => {
   fs.writeFileSync(getSystemConfigPath(serviceName), JSON.stringify(serviceConfigData, null, 2));
   dataConfig[serviceName] = serviceConfigData;
};
