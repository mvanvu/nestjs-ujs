import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDService } from '@microservice/@library';
import { CreateRestaurantDto, UpdateRestaurantDto } from '../dto';
import { RestaurantEntity } from '../entity';

@Injectable()
export class RestaurantService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Restaurant')
         .options({ list: { filterFields: ['ownerId'] } })
         .validateDTOPipe(CreateRestaurantDto, UpdateRestaurantDto)
         .entityResponse(RestaurantEntity);
   }
}
