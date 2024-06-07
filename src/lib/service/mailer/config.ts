import { Registry } from '@mvanvu/ujs';
import { TransporterList } from './type/transporter';
const envConfig = Registry.from(process.env);
const smtpPort = envConfig.get('MAILER_SMTP_PORT', '465', 'toUInt');

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
         host: envConfig.get('MAILER_SMTP_HOST'),
         port: smtpPort, // 587
         secure: smtpPort === 465, // Use true for port 465, false for all other ports
         auth: {
            user: envConfig.get('MAILER_SMTP_USER'),
            pass: envConfig.get('MAILER_SMTP_PASS'),
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
