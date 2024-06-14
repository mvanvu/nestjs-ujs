import { Module } from '@nestjs/common';
import { ContentCategoryController, ContentPostController, ContentTagController } from './controller';

@Module({
   controllers: [ContentCategoryController, ContentPostController, ContentTagController],
})
export class ContentModule {}
