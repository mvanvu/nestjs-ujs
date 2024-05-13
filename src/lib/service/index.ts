import { Registry } from '@mvanvu/ujs';

// Service shared lib
export * from './storage';
export * from './user';

// Service shared config
import user from './user/config';
import storage from './storage/config';
import { loadPermissionKeys } from '@lib/util';

export default Registry.from({ user, storage });

export const clientProxies = [user.proxy, storage.proxy];

// Load all permission keys
export const permissionKeys: string[] = [];
loadPermissionKeys(user.permissions ?? {}, permissionKeys);
loadPermissionKeys(storage.permissions ?? {}, permissionKeys);
