import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDService } from '@microservice/@library';
import { CreateTableDto, UpdateTableDto } from '../dto';
import { TableEntity } from '../entity';

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
