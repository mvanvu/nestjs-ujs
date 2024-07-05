import { SYSTEM_GET_CONFIG_PATTERN } from '@shared-library';

export default {
   name: 'system',
   patterns: {
      getConfig: SYSTEM_GET_CONFIG_PATTERN,
      saveConfig: 'system.saveConfig',
      getActivityLog: 'system.getActivityLog',
      writeActivityLog: 'system.writeActivityLog',
      sendMail: 'system.sendMail',
      upload: 'system.storage.upload',
   },
   permissions: {
      activityLog: {
         get: 'system.activityLog.get',
      },
      config: {
         get: 'system.config.get',
         save: 'system.config.save',
      },
      storage: {
         upload: 'system.storage.upload',
      },
      mail: { test: 'system.mail.sendTest' },
   },
   upload: {
      prefix: 'assets',
      localPath: 'storage',
      provider: 'Local',
      googleDriveCredentialsPath: 'credentials/google-drive.json',
   },
} as const;
