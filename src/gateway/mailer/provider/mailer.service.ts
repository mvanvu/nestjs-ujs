import { OnEvent } from '@gateway/lib';
import { OnServiceResponse, eventConstant } from '@lib/common';
import { UserEntity } from '@lib/service';
import { SendMailDto } from '@lib/service/mailer';
import { serviceConfig } from '@metadata';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';
import * as fs from 'fs';

type MailerTemplate = { index: string; resetPasswordBody: string; verifyAccountBody: string };

@Injectable()
export class MailerService {
   private static TEMPLATES: MailerTemplate;

   @Inject(serviceConfig.get('mailer.name').toUpperCase() + '_MICROSERVICE')
   private readonly clientProxy: ClientProxy;

   get templates(): MailerTemplate {
      if (!MailerService.TEMPLATES) {
         const mailerTmplPath = process.cwd() + '/src/gateway/mailer/template';
         MailerService.TEMPLATES = {
            index: fs.readFileSync(`${mailerTmplPath}/index.html`).toString('utf8'),
            resetPasswordBody: fs.readFileSync(`${mailerTmplPath}/body/reset-password.html`).toString('utf8'),
            verifyAccountBody: fs.readFileSync(`${mailerTmplPath}/body/verify-account.html`).toString('utf8'),
         };
      }

      return MailerService.TEMPLATES;
   }

   @OnEvent(eventConstant.onServiceResponse)
   onServiceResponse(payload: OnServiceResponse): void {
      switch (payload.messagePattern) {
         case serviceConfig.get('user.patterns.signUp'):
            return this.sendVerifyAccountCode(payload);

         case serviceConfig.get('user.patterns.updateResetPasswordCode'):
            return this.sendVerifyResetPasswordCode(payload);
      }
   }

   parseBody(innerBody: string, contentData?: Record<string, string>): string {
      if (contentData) {
         for (const key in contentData) {
            innerBody = innerBody.replace(new RegExp(`\\{{2}\\s*${key}\\s*\\}{2}`, 'g'), contentData[key]);
         }
      }

      return this.templates.index.replace('{{ body }}', innerBody);
   }

   sendVerifyAccountCode(payload: OnServiceResponse): void {
      const user = payload.responseData as UserEntity;

      if (user.email && user.verifyCode?.activateAccount) {
         const code = user.verifyCode.activateAccount;
         const data: SendMailDto = {
            to: [user.email],
            subject: 'Verify your account',
            body: this.parseBody(this.templates.verifyAccountBody, {
               name: user.name || user.username || user.email,
               url: `${serviceConfig.get('user.httpWebVerifyAccountUrl').replace(/\/+$/, '')}?code=${code}`,
               code,
            }),
         };

         this.clientProxy.emit(serviceConfig.get('mailer.patterns.send'), data);
      }

      // Remove verify code
      if (user.verifyCode) {
         delete user.verifyCode;
      }
   }

   sendVerifyResetPasswordCode(payload: OnServiceResponse): void {
      const user = payload.responseData as false | UserEntity;

      if (user) {
         const code = user.verifyCode.resetPassword;
         const data: SendMailDto = {
            to: [user.email],
            subject: 'Reset your password',
            body: this.parseBody(this.templates.resetPasswordBody, {
               name: user.name || user.username || user.email,
               url: `${serviceConfig.get('user.httpWebVerifyResetPwdUrl').replace(/\/+$/, '')}?code=${code}`,
               code,
            }),
         };

         this.clientProxy.emit(serviceConfig.get('mailer.patterns.send'), data);
      }
   }
}
