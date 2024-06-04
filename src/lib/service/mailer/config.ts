import { TransporterList } from './type/transporter';

export default {
   name: 'mailer',
   patterns: {
      send: 'mailer.send',
      sendTest: 'mailer.sendTest',
   },
   transporter: {
      default: 'smtp' as TransporterList,
      smtp: {
         pool: true,
         host: 'smtp.gmail.com',
         port: 465, // 587
         secure: true, // Use true for port 465, false for all other ports
         auth: {
            user: 'GMAIL_ACCOUNT',
            pass: 'GMAIL_PASSWORD',
         },
      },
   },
   test: {
      smtp: {
         host: 'smtp.ethereal.email',
         port: 587,
         auth: {
            user: 'hailie.lowe@ethereal.email',
            pass: '7CJ62DVaUhymtrnzkS',
         },
      },
   },
} as const;
