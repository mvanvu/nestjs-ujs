import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/user';
import { CreatePrismaService } from '@microservice/@library';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, Prisma.dmmf.datamodel.models as any[]) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
