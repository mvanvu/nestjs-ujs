export const loadPermissionKeys = (permission?: object | string, permissionKeys?: string[]): string[] => {
   if (!permissionKeys) {
      permissionKeys = [];
   }

   if (typeof permission === 'string') {
      permissionKeys.push(permission);
   } else if (typeof permission === 'object' && permission !== null) {
      for (const prop in permission) {
         loadPermissionKeys(permission[prop], permissionKeys);
      }
   }

   return permissionKeys;
};
