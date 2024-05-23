import { Registry } from '@mvanvu/ujs';

// Service shared lib
export * from './storage';
export * from './user';

// Service shared config
import user from './user/config';
import storage from './storage/config';
import { loadPermissionKeys } from '@lib/common/util';

const serviceConfig = Registry.from({ user, storage }, { consistent: true });
export const serviceListNames: string[] = [serviceConfig.get('user.name'), serviceConfig.get('storage.name')];

// Load all permission keys
export const permissionKeys: string[] = [];
loadPermissionKeys(user.permissions, permissionKeys);
loadPermissionKeys(storage.permissions, permissionKeys);

export default serviceConfig;
