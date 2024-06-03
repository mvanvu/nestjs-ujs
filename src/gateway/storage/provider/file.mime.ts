const imageMimeTypes = [
   'image/jpeg',
   'image/png',
   'image/gif',
   'image/bmp',
   'image/svg+xml',
   'image/webp',
   'image/tiff',
   'image/vnd.microsoft.icon',
];

export const videoMimeTypes = [
   'video/mp4',
   'video/webm',
   'video/x-msvideo',
   'video/quicktime',
   'video/x-ms-wmv',
   'video/x-flv',
   'video/x-matroska',
];

export const documentMimeTypes = [
   'application/pdf',
   'application/msword',
   'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
   'application/vnd.ms-excel',
   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
   'application/vnd.ms-powerpoint',
   'application/vnd.openxmlformats-officedocument.presentationml.presentation',
   'text/plain',
   'application/rtf',
   'text/csv',
   'text/markdown',
   'application/xml',
   'text/xml',
   'text/html',
];

export const compressMimeTypes = ['application/zip', 'application/gzip', 'application/x-tar', 'application/vnd.rar'];

export const fileTypes = {
   image: imageMimeTypes,
   video: videoMimeTypes,
   media: [...imageMimeTypes, ...videoMimeTypes],
   document: [...documentMimeTypes, ...compressMimeTypes],
};

export const allAcceptedFileTypes = [...fileTypes.image, ...fileTypes.video, ...fileTypes.media, ...fileTypes.document];
