export default {
   proxy: 'UserProxy',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      verify: 'user.verify',
      readUser: 'user.CRUD.read',
      writeUser: 'user.CRUD.write',
      deleteUser: 'user.CRUD.delete',
      readRole: 'role.CRUD.read',
      writeRole: 'role.CRUD.write',
      deleteRole: 'role.CRUD.delete',
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
