import { Inject, Injectable } from '@nestjs/common';
import { CategoryService } from '../provider';
import { serviceConfig } from '@metadata';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@lib/common';
import { CategoryEntity } from '@lib/service/order';
const patterns = serviceConfig.get('order.patterns');

@Injectable()
export class CategoryController {
   @Inject(CategoryService) readonly categoryService: CategoryService;

   @MessagePattern(patterns.categoryCRUD)
   executeCRUD(): Promise<CRUDResult<CategoryEntity>> {
      return this.categoryService.createCRUDService().execute();
   }
}
