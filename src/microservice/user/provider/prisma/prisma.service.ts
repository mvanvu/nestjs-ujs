import { Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/user';
import { userDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, userDataModels) {}
