export default {
   proxy: 'StorageProxy',
   patterns: {
      upload: 'storage.upload',
   },
   permissions: {
      file: {
         upload: 'file.upload',
      },
   },
};
