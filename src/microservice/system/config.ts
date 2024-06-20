import { SYSTEM_GET_CONFIG_PATTERN, SYSTEM_PERMISSION_ADMIN_SCOPE } from '@shared-library';

export default {
   name: 'system',
   patterns: {
      getConfig: SYSTEM_GET_CONFIG_PATTERN,
      saveConfig: 'system.saveConfig',
      getActivityLog: 'system.getActivityLog',
      writeActivityLog: 'system.writeActivityLog',
   },
   permissions: {
      admin: {
         scope: SYSTEM_PERMISSION_ADMIN_SCOPE,
      },
      activityLog: {
         get: 'system.activityLog.get',
      },
      config: {
         get: 'system.config.get',
         save: 'system.config.save',
      },
   },
} as const;
