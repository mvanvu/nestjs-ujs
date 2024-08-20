import { BaseService, CRUDService, CreateCRUDService } from '@microservice/@library';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { FieldsException } from '@shared-library';
import { Is, Transform } from '@mvanvu/ujs';
import { CategoryEntity } from '../entity';
import { UpdateCategoryDto } from '../dto';
import { Prisma, Category } from '.prisma/content';

@Injectable()
export class CategoryService extends BaseService implements CreateCRUDService<PrismaService> {
   @Inject(PrismaService) readonly prisma: PrismaService;

   async rebuildPath(): Promise<Array<Category & { paths: string[] }>> {
      const categories = await this.prisma.category.findMany();
      const buildPath = (category: Category & { paths?: string[] }) => {
         if (!category.paths) {
            category.paths = [];
         }

         category.paths.push(category.slug);
         let parent = category.parentId ? categories.find(({ id }) => id === category.parentId) : null;

         while (parent) {
            category.paths.push(parent.slug);

            if (parent.parentId) {
               parent = categories.find(({ id }) => id === parent.parentId);
            } else {
               break;
            }
         }

         return category as Category & { paths: string[] };
      };

      await this.prisma.$transaction(async (tx) => {
         const promises = [];

         for (const category of categories) {
            const { id, paths } = buildPath(category);
            promises.push(tx.category.update({ where: { id }, data: { path: paths.reverse().join('/') } }));
         }

         await Promise.all(promises);
      });

      return categories as Array<Category & { paths: string[] }>;
   }

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Category')
         .entityResponse(CategoryEntity)
         .options({ list: { searchFields: ['title', 'description'] } })
         .include<Prisma.CategoryInclude>({
            parent: { select: { id: true, status: true, title: true, slug: true, path: true } },
         })
         .beforeExecute<CategoryEntity, UpdateCategoryDto, 'create' | 'update'>(async ({ context, record, data }) => {
            if (context !== 'create' && context !== 'update') {
               return;
            }

            const fieldsException = new FieldsException();

            if (!data.slug && context === 'create') {
               data.slug = Transform.toSlug(data.title);
            }

            if (Is.empty(data.slug)) {
               fieldsException.add('slug', FieldsException.BAD_REQUEST, this.language._('CONTENT_EMPTY_SLUG_WARN'));
            } else {
               const findBySlug = await this.prisma.category.findFirst({
                  where: { slug: data.slug, id: context === 'update' ? { not: record.id } : undefined },
               });

               if (findBySlug) {
                  fieldsException.add(
                     'slug',
                     FieldsException.ALREADY_EXISTS,
                     this.language._('CONTENT_$SLUG_READY_EXISTS', { slug: data.slug }),
                  );
               }
            }

            if (data.parentId && !(await this.prisma.category.findUnique({ where: { id: data.parentId } }))) {
               fieldsException.add(
                  'parentId',
                  FieldsException.NOT_FOULND,
                  this.language._('CONTENT_CATEGORY_$ID_NOT_EXISTS', { id: data.parentId }),
               );
            }

            fieldsException.validate();
         })
         .afterTrabsaction(async ({ record }) => {
            const target = (await this.rebuildPath()).find(({ id }) => id === record.id);
            record.path = target.paths.reverse().join('/');
         });
   }
}
