export default {
   name: 'order',
   patterns: {
      categoryCRUD: 'order.category.CRUD',
      itemCRUD: 'order.item.CRUD',
      restaurantCRUD: 'order.restaurant.CRUD',
      staffCRUD: 'order.staff.CRUD',
      tableCRUD: 'order.table.CRUD',
   },
   permissions: {
      category: {
         read: 'order.category.read',
         create: 'order.category.create',
         update: 'order.category.update',
         delete: 'order.category.delete',
      },
      restaurant: {
         read: 'order.restaurant.read',
         create: 'order.restaurant.create',
         update: 'order.restaurant.update',
         delete: 'order.restaurant.delete',
      },
   },
} as const;
