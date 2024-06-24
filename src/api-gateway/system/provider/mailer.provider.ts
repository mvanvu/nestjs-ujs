import { OnEvent } from '@gateway/@library';
import { DataMetaResult, OnServiceResponse, UserEntity, eventConstant } from '@shared-library';
import { injectProxy, serviceConfig } from '@metadata';
import { Inject, Injectable } from '@nestjs/common';
import { ClientProxy } from '@nestjs/microservices';
import * as fs from 'fs';
import { SendMailDto } from '@microservice/system/dto';

export type MailerTemplate = { index: string; resetPasswordBody: string; verifyAccountBody: string };
const mailerTmplPath = process.cwd() + '/src/api-gateway/system/provider/mailer-template';
const { name, patterns } = serviceConfig.get('system');

@Injectable()
export class MailerProvider {
   private readonly templates: MailerTemplate = {
      index: fs.readFileSync(`${mailerTmplPath}/index.html`).toString('utf8'),
      resetPasswordBody: fs.readFileSync(`${mailerTmplPath}/body/reset-password.html`).toString('utf8'),
      verifyAccountBody: fs.readFileSync(`${mailerTmplPath}/body/verify-account.html`).toString('utf8'),
   };

   @Inject(injectProxy(name))
   private readonly systemProxy: ClientProxy;

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

   sendVerifyAccountCode(
      payload: OnServiceResponse<DataMetaResult<UserEntity, { verifyCode: { activateAccount?: string } }>>,
   ): void {
      if (!payload.success) {
         return;
      }

      const { data: user, meta } = payload.responseData;

      if (user.email && meta?.verifyCode?.activateAccount) {
         const code = meta.verifyCode.activateAccount;
         const data: SendMailDto = {
            to: [user.email],
            subject: 'Verify your account',
            body: this.parseBody(this.templates.verifyAccountBody, {
               name: user.name || user.username || user.email,
               url: `${serviceConfig.get('user.httpWebVerifyAccountUrl').replace(/\/+$/, '')}?code=${code}`,
               code,
            }),
         };

         this.systemProxy.emit(patterns.sendMail, data);
      }
   }

   sendVerifyResetPasswordCode(
      payload: OnServiceResponse<DataMetaResult<UserEntity, { verifyCode: { resetPassword?: string } }>>,
   ): void {
      if (!payload.success) {
         return;
      }

      const { data: user, meta } = payload.responseData;

      if (user?.email && meta?.verifyCode?.resetPassword) {
         const code = meta.verifyCode.resetPassword;

         if (code) {
            const data: SendMailDto = {
               to: [user.email],
               subject: 'Reset your password',
               body: this.parseBody(this.templates.resetPasswordBody, {
                  name: user.name || user.username || user.email,
                  url: `${serviceConfig.get('user.httpWebVerifyResetPwdUrl').replace(/\/+$/, '')}?code=${code}`,
                  code,
               }),
            };

            this.systemProxy.emit(patterns.sendMail, data);
         }
      }
   }
}
