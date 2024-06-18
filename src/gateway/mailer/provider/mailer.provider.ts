import { OnEvent } from '@gateway/lib';
import { MetaResult, OnServiceResponse, eventConstant } from '@lib';
import { UserEntity } from '@service';
import { SendMailDto } from '@service/mailer';
import { serviceConfig } from '@metadata';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';
import * as fs from 'fs';

export type MailerTemplate = { index: string; resetPasswordBody: string; verifyAccountBody: string };
const mailerTmplPath = process.cwd() + '/src/gateway/mailer/template';

@Injectable()
export class MailerProvider {
   private readonly templates: MailerTemplate = {
      index: fs.readFileSync(`${mailerTmplPath}/index.html`).toString('utf8'),
      resetPasswordBody: fs.readFileSync(`${mailerTmplPath}/body/reset-password.html`).toString('utf8'),
      verifyAccountBody: fs.readFileSync(`${mailerTmplPath}/body/verify-account.html`).toString('utf8'),
   };

   @Inject(serviceConfig.get('mailer.name').toUpperCase() + '_MICROSERVICE')
   private readonly clientProxy: ClientProxy;

   @OnEvent(eventConstant.onServiceResponse)
   onServiceResponse(payload: OnServiceResponse): void {
      if (!payload.success) {
         return;
      }

      switch (payload.messagePattern) {
         case serviceConfig.get('user.patterns.signUp'):
            return this.sendVerifyAccountCode(payload);

         case serviceConfig.get('user.patterns.sendResetPasswordCode'):
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
      const { data: user, meta } = payload.responseData as MetaResult<UserEntity>;

      if (user.email && meta.verifyCode) {
         const code = meta.verifyCode;
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
   }

   sendVerifyResetPasswordCode(payload: OnServiceResponse): void {
      const response = payload.responseData as false | MetaResult<UserEntity>;

      if (response) {
         const { data: user, meta } = response;
         const code = meta.verifyCode;
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
