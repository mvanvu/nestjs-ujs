export default {
   name: 'storage',
   patterns: {
      upload: 'storage.upload',
   },
   permissions: {
      upload: 'storage.upload',
   },
   upload: {
      prefix: 'assets',
      localPath: 'storage',
      provider: 'Local',
      googleDriveCredentialsPath: 'credentials/google-drive.json',
   },
} as const;
