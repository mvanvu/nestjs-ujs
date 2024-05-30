import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/storage';
import { storageDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, storageDataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
