import { BaseService, CRUDService, CreateCRUDService } from '@microservice/@library';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { FieldsException } from '@shared-library';
import { Is, Transform } from '@mvanvu/ujs';
import { CategoryRef, PostEntity } from '../entity';
import { UpdatePostDto } from '../dto';
import { Prisma } from '.prisma/content';

@Injectable()
export class TagService extends BaseService implements CreateCRUDService<PrismaService> {
   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Tag')
         .entityResponse(PostEntity)
         .options({ list: { searchFields: ['title'] } });
   }
}
