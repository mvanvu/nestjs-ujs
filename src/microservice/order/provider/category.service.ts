import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDService } from '@microservice/@library';
import { CreateCategoryDto, UpdateCategoryDto } from '../dto';
import { CategoryEntity } from '../entity';

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
