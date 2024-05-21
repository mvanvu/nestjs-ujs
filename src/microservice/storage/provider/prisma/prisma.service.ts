import { Injectable } from '@nestjs/common';
import { PrismaClient } from '.prisma/storage';
import { storageDataModels } from './prisma.datamodel';
import { CreatePrismaService } from '@service/lib';

@Injectable()
export class PrismaService extends CreatePrismaService(PrismaClient, storageDataModels) {}
