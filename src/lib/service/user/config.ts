export default {
   name: 'user',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      verify: 'user.verify',
      deleteSeft: 'user.deleteSeft',
      userCRUD: 'user.CRUD',
      roleCRUD: 'role.CRUD',
   },
   permissions: {
      role: {
         create: 'role.create',
         read: 'role.read',
         update: 'role.update',
         delete: 'role.delete',
      },
      user: {
         create: 'user.create',
         read: 'user.read',
         update: 'user.update',
         delete: 'user.delete',
      },
   },
};
