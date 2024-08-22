import { BaseService } from '@microservice/@library';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { TagEntity } from '../entity';
import { CreateTagDto, UpdateTagDto } from '../dto';

@Injectable()
export class TagService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService() {
      return this.prisma
         .createCRUDService('tag', { entity: TagEntity, createDto: CreateTagDto, updateDto: UpdateTagDto })
         .options({ list: { searchFields: ['title'] } })
         .afterUpdate(async ({ tx, record }) => {
            await tx.post.updateMany({
               data: {
                  tags: {
                     updateMany: {
                        data: { id: record.id, title: record.title, status: record.status },
                        where: { id: record.id },
                     },
                  },
               },
               where: { tags: { some: { id: record.id } } },
            });
         })
         .afterDelete(async ({ tx, record }) => {
            await tx.post.updateMany({
               data: { tags: { deleteMany: { where: { id: record.id } } } },
               where: { tags: { some: { id: record.id } } },
            });
         });
   }
}
