import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateTableDto, TableEntity, UpdateTableDto } from '@service/order';
import { CRUDService } from '@service/lib';

@Injectable()
export class TableService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Table')
         .options({ list: { filterFields: ['ownerId[restaurant.owner.id]', 'restaurantId'] } })
         .validateDTOPipe(CreateTableDto, UpdateTableDto)
         .entityResponse(TableEntity);
   }
}
