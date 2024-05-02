export const userConfig = {
   proxy: 'UserProxy',
   patterns: {
      signIn: 'user.signIn',
      signUp: 'user.signUp',
      verify: 'user.verify',
      paginateUser: 'user.CRUD.paginate',
      readUser: 'user.CRUD.read',
      createUser: 'user.create',
      updateUser: 'user.update',
      deleteUser: 'user.delete',
      rolePaginate: 'role.CRUD.paginate',
      roleRead: 'role.CRUD.read',
      roleCreate: 'role.CRUD.create',
      roleUpdate: 'role.CRUD.update',
      roleDelete: 'role.CRUD.delete',
   },
};

export const rolePermissions = {
   role: {
      create: 'ROLE.CREATE',
      read: 'ROLE.READ',
      update: 'ROLE.UPDATE',
      delete: 'ROLE.DELETE',
   },
};

export const permissionKeys: string[] = [];

function loadKeys(permission: object | string) {
   if (typeof permission === 'string') {
      permissionKeys.push(permission);
   } else if (typeof permission === 'object' && permission !== null) {
      for (const prop in permission) {
         loadKeys(permission[prop]);
      }
   }
}

loadKeys(rolePermissions);
