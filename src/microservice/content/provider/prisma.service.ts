import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/content';
import { CreatePrismaService } from '@microservice/@library';
import { MessageMetaProvider } from '@shared-library';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, Prisma.dmmf.datamodel.models) {
   @Inject(MessageMetaProvider) readonly meta: MessageMetaProvider;
}
