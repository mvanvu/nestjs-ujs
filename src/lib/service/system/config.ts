export default {
   name: 'system',
   patterns: {
      saveConfig: 'system.saveConfig',
      writeActivityLog: 'system.writeActivityLog',
   },
   permissions: {
      config: {
         save: 'system.config.save',
      },
   },
} as const;
