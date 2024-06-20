import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDService } from '@microservice/@library';
import { CreateItemDto, UpdateItemDto } from '../dto';
import { ItemEntity } from '../entity';

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
