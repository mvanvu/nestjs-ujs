export default {
   name: 'content',
   patterns: {
      categoryCRUD: 'content.category.CRUD',
      postCRUD: 'content.post.CRUD',
      tagCRUD: 'content.tag.CRUD',
   },
   permissions: {
      category: {
         read: 'content.category.read',
         create: 'content.category.create',
         update: 'content.category.update',
         delete: 'content.category.delete',
      },
      post: {
         read: 'content.post.read',
         create: 'content.post.create',
         update: 'content.post.update',
         delete: 'content.post.delete',
      },
      tag: {
         read: 'content.tag.read',
         create: 'content.tag.create',
         update: 'content.tag.update',
         delete: 'content.tag.delete',
      },
   },
} as const;
