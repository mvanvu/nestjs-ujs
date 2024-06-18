export default {
   name: 'system',
   patterns: {
      saveConfig: 'system.saveConfig',
      getConfig: 'system.getConfig',
      getActivityLog: 'system.getActivityLog',
      writeActivityLog: 'system.writeActivityLog',
   },
   permissions: {
      admin: {
         scope: 'system.admin.scope',
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
