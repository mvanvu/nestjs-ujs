export default {
   proxy: 'UserProxy',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      verify: 'user.verify',
      userCRUD: 'user.CRUD',
      roleCRUD: 'role.CRUD',
   },
   permissions: {
      role: {
         create: 'role.create',
         read: 'role.read',
         update: 'role.update',
         updateOwn: 'role.update.own',
         delete: 'role.delete',
         deleteOwn: 'role.delete.own',
      },
   },
};
