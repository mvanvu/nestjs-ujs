import { Controller, Inject } from '@nestjs/common';
import { CategoryService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { CategoryEntity } from '../entity';
const patterns = serviceConfig.get('order.patterns');

@Controller()
export class CategoryController {
   @Inject(CategoryService) readonly categoryService: CategoryService;

   @MessagePattern(patterns.categoryCRUD)
   executeCRUD(): Promise<CRUDResult<CategoryEntity>> {
      return this.categoryService.createCRUDService().execute();
   }
}
