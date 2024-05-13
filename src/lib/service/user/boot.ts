import { BootService } from '../boot';
export default new BootService({
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
   permissions: {
      role: {
         create: 'ROLE.CREATE',
         read: 'ROLE.READ',
         update: 'ROLE.UPDATE',
         delete: 'ROLE.DELETE',
      },
   },
});
