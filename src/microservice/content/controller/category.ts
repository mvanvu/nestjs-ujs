import { serviceConfig } from '@metadata';
import { Controller, Inject } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { CategoryEntity } from '../entity';
import { CategoryService } from '../provider';
const { patterns } = serviceConfig.get('content');

@Controller()
export class CategoryController {
   @Inject(CategoryService) private readonly categoryService: CategoryService;

   @MessagePattern(patterns.categoryCRUD)
   executeCRUD(): Promise<CRUDResult<CategoryEntity>> {
      return this.categoryService.createCRUDService().execute();
   }
}
