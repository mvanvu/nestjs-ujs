import { Injectable } from '@nestjs/common';
import { PrismaClient, Prisma } from '.prisma/user';
import { CreatePrismaService } from '@service/lib';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, Prisma.dmmf.datamodel.models) {}
