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
         .include<Prisma.PostInclude>({
            category: { select: { id: true, status: true, name: true, slug: true, path: true } },
         })
         .beforeExecute<PostEntity, UpdatePostDto, 'create' | 'update'>(async ({ context, record, data }) => {
            if (context !== 'create' && context !== 'update') {
               return;
            }

            const fieldsException = new FieldsException();
            const isUpdate = context === 'update';

            if (!data.slug && !isUpdate) {
               data.slug = Transform.toPath(data.name).toLowerCase().replace(/\/+/, '-');
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

            fieldsException.validate();

            // Build path
            data['path'] = (category ? category.path + '/' : '') + data.slug;
         });
   }
}
