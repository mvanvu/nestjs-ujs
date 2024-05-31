export default {
   name: 'user',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      verify: 'user.verify',
      deleteSelf: 'user.deleteSelf',
      userCRUD: 'user.CRUD',
      groupCRUD: 'group.CRUD',
      roleCRUD: 'role.CRUD',
   },
   permissions: {
      group: {
         create: 'group.create',
         read: 'group.read',
         update: 'group.update',
         delete: 'group.delete',
      },
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
} as const;
