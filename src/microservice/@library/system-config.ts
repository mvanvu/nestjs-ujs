import { appConfig } from '@metadata';
import { SystemConfigDto } from '@shared-library';
import * as fs from 'fs';

let dataConfig: SystemConfigDto = null;
const systemConfigPath = `${process.cwd()}/src/microservice/${appConfig.get('appEnv')}/system.config.json`;

export const getSystemConfig = (): SystemConfigDto => {
   if (!dataConfig) {
      dataConfig = fs.existsSync(systemConfigPath) ? JSON.parse(fs.readFileSync(systemConfigPath).toString()) : {};
   }

   return dataConfig;
};

export const updateSystemConfig = (serviceConfigData: SystemConfigDto): SystemConfigDto => {
   dataConfig = serviceConfigData;
   fs.writeFileSync(systemConfigPath, JSON.stringify(dataConfig, null, 2));

   return dataConfig;
};
