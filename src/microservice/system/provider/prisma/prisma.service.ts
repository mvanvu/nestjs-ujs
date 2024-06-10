import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/system';
import { systemDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, systemDataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
