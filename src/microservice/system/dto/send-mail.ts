import { Schema } from '@mvanvu/ujs';
import { ApiProperty } from '@nestjs/swagger';

export class SendMailDto {
   @Schema.string().optional().decorate()
   from?: string;

   @Schema.email().array().decorate()
   to: string[];

   @Schema.content().decorate()
   subject: string;

   @Schema.raw().decorate()
   body: string;
}

export class SendTestMailDto {
   @Schema.email().decorate()
   email: string;

   @ApiProperty({ example: 'Test send email' })
   @Schema.content().decorate()
   subject: string;

   @ApiProperty({ example: 'Send test mail successfully, this email for testing purpose' })
   @Schema.raw().decorate()
   body: string;
}
