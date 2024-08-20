import { serviceConfig } from '@metadata';
import { Controller, Inject } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { CategoryEntity } from '../entity';
import { PostService, TagService } from '../provider';
const { patterns } = serviceConfig.get('content');

@Controller()
export class TagController {
   @Inject(TagService) private readonly tagService: TagService;

   @MessagePattern(patterns.tagCRUD)
   executeCRUD(): Promise<CRUDResult<CategoryEntity>> {
      return this.tagService.createCRUDService().execute();
   }
}
