import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateRestaurantDto, RestaurantEntity, UpdateRestaurantDto } from '@lib/service/order';
import { CRUDService } from '@service/lib';

@Injectable()
export class RestaurantService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Restaurant')
         .validateDTOPipe(CreateRestaurantDto, UpdateRestaurantDto)
         .entityResponse(RestaurantEntity);
   }
}
