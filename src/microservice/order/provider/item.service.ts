import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CreateItemDto, ItemEntity, UpdateItemDto } from '@lib/microservice/order';
import { CRUDService } from '@microservice/lib';

@Injectable()
export class ItemService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Item')
         .validateDTOPipe(CreateItemDto, UpdateItemDto)
         .entityResponse(ItemEntity);
   }
}
