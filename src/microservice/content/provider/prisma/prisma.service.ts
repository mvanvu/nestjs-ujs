import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/content';
import { contentDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, contentDataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
