import { BaseService } from '@microservice/@library';
import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { FieldsException } from '@shared-library';
import { Is, Transform } from '@mvanvu/ujs';
import { PostEntity } from '../entity';
import { CreatePostDto, UpdatePostDto } from '../dto';
import { Prisma } from '.prisma/content';

@Injectable()
export class PostService extends BaseService {
   @Inject(PrismaService) readonly prisma: PrismaService;

   private async validateData(fieldsException: FieldsException, data: UpdatePostDto): Promise<void> {
      if (data.slug && (await this.prisma.post.findFirst({ where: { slug: data.slug } }))) {
         fieldsException.add(
            'slug',
            FieldsException.ALREADY_EXISTS,
            this.language._('CONTENT_$SLUG_READY_EXISTS', { slug: data.slug }),
         );
      }

      if (data.categoryId) {
         const category = await this.prisma.category.findUnique({ where: { id: data.categoryId } });

         if (category) {
            Object.assign(data, { path: category.path + '/' + data.slug });
         } else {
            fieldsException.add(
               'categoryId',
               FieldsException.NOT_FOULND,
               this.language._('CONTENT_CATEGORY_$ID_NOT_EXISTS', { id: data.categoryId }),
            );
         }
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
   }

   createCRUDService() {
      return this.prisma
         .createCRUDService('post', { entity: PostEntity, createDto: CreatePostDto, updateDto: UpdatePostDto })
         .options({ list: { searchFields: ['title', 'description'] } })
         .include<Prisma.PostInclude>({
            category: { select: { id: true, status: true, title: true, slug: true, path: true } },
         })
         .beforeCreate(async ({ data }) => {
            const fieldsException = new FieldsException();

            if (!data.slug) {
               data.slug = Transform.toSlug(data.title);
            }

            if (Is.empty(data.slug)) {
               fieldsException.add('slug', FieldsException.BAD_REQUEST, this.language._('CONTENT_EMPTY_SLUG_WARN'));
            }

            await this.validateData(fieldsException, data);

            if (!data['path']) {
               Object.assign(data, { path: data.slug });
            }

            if (!data['tags']) {
               Object.assign(data, { tags: [] });
            }
         })
         .beforeUpdate(async ({ data, record }) => {
            if (data.slug && data.slug === record.slug) {
               delete data.slug;
            }

            await this.validateData(new FieldsException(), data);
         });
   }
}
