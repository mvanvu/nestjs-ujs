import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CategoryEntity, CreateCategoryDto, UpdateCategoryDto } from '@lib/microservice/order';
import { CRUDService } from '@microservice/lib';

@Injectable()
export class CategoryService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Category')
         .validateDTOPipe(CreateCategoryDto, UpdateCategoryDto)
         .entityResponse(CategoryEntity);
   }
}
