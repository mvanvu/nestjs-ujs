import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/storage';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

const dataModels = {};
Prisma.dmmf.datamodel.models.forEach((model) => (dataModels[model.name] = model));

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, dataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
