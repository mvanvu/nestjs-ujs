import { Inject, Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/user';
import { userDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';
import { CONTEXT, RequestContext } from '@nestjs/microservices';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, userDataModels) {
   @Inject(CONTEXT) readonly ctx: RequestContext;
}
