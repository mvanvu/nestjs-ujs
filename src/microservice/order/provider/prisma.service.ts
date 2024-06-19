import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/order';
import { CreatePrismaService } from '@microservice/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, Prisma.dmmf.datamodel.models as any[]) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
