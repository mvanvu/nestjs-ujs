import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/order';
import { orderDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, orderDataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
