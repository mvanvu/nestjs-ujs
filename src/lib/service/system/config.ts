export default {
   name: 'system',
   patterns: {
      saveConfig: 'system.saveConfig',
      getConfig: 'system.getConfig',
      writeActivityLog: 'system.writeActivityLog',
   },
   permissions: {
      config: {
         get: 'system.config.get',
         save: 'system.config.save',
      },
   },
} as const;
