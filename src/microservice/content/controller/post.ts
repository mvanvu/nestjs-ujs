import { serviceConfig } from '@metadata';
import { Controller, Inject } from '@nestjs/common';
import { MessagePattern } from '@nestjs/microservices';
import { CRUDResult } from '@shared-library';
import { CategoryEntity } from '../entity';
import { PostService } from '../provider';
const { patterns } = serviceConfig.get('content');

@Controller()
export class PostController {
   @Inject(PostService) private readonly postService: PostService;

   @MessagePattern(patterns.postCRUD)
   executeCRUD(): Promise<CRUDResult<CategoryEntity>> {
      return this.postService.createCRUDService().execute();
   }
}
