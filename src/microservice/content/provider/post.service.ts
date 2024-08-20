import { BaseService, CRUDService, CreateCRUDService } from '@microservice/@library';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { FieldsException } from '@shared-library';
import { Is, Transform } from '@mvanvu/ujs';
import { CategoryRef, PostEntity } from '../entity';
import { UpdatePostDto } from '../dto';
import { Prisma } from '.prisma/content';

@Injectable()
export class PostService extends BaseService implements CreateCRUDService<PrismaService> {
   @Inject(PrismaService) readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Post')
         .entityResponse(PostEntity)
         .options({ list: { searchFields: ['title', 'description'] } })
         .include<Prisma.PostInclude>({
            category: { select: { id: true, status: true, title: true, slug: true, path: true } },
         })
         .beforeExecute<PostEntity, UpdatePostDto, 'create' | 'update'>(async ({ context, record, data }) => {
            if (context !== 'create' && context !== 'update') {
               return;
            }

            const fieldsException = new FieldsException();
            const isUpdate = context === 'update';

            if (!data.slug) {
               data.slug = isUpdate ? record.slug : Transform.toSlug(data.title);
            }

            if (Is.empty(data.slug)) {
               fieldsException.add('slug', FieldsException.BAD_REQUEST, this.language._('CONTENT_EMPTY_SLUG_WARN'));
            } else {
               const findBySlug = await this.prisma.post.findFirst({
                  where: { slug: data.slug, id: isUpdate ? { not: record.id } : undefined },
               });

               if (findBySlug) {
                  fieldsException.add(
                     'slug',
                     FieldsException.ALREADY_EXISTS,
                     this.language._('CONTENT_$SLUG_READY_EXISTS', { slug: data.slug }),
                  );
               }
            }

            let category: CategoryRef = isUpdate ? record.category : undefined;

            if (data.categoryId) {
               category = await this.prisma.category.findUnique({ where: { id: data.categoryId } });

               if (!category) {
                  fieldsException.add(
                     'categoryId',
                     FieldsException.NOT_FOULND,
                     this.language._('CONTENT_CATEGORY_$ID_NOT_EXISTS', { id: data.categoryId }),
                  );
               }
            } else if (isUpdate && data.categoryId === null) {
               category = undefined;
            }

            // Tags validation
            if (data.tags?.length) {
               const tags = await this.prisma.tag.findMany({
                  where: { id: { in: data.tags } },
                  select: { id: true, title: true, status: true },
               });

               for (let i = 0, n = data.tags.length; i < n; i++) {
                  const tagId = data.tags[i];

                  if (!tags.find((tag) => tag.id === tagId)) {
                     fieldsException.add(
                        `tags[${i}]`,
                        FieldsException.NOT_FOULND,
                        this.language._('CONTENT_TAG_$ID_NOT_EXISTS', { id: tagId }),
                     );
                  }
               }

               Object.assign(data, { tags });
            } else if (data.tags === null) {
               data.tags = [];
            }

            fieldsException.validate();

            // Build path
            data['path'] = (category ? category.path + '/' : '') + data.slug;
         });
   }
}
