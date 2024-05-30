import { Registry } from '@mvanvu/ujs';

// Service shared lib
export * from './storage';
export * from './user';

// Service shared config
import user from './user/config';
import storage from './storage/config';
import { loadPermissionKeys } from '@lib/common/util';

const serviceConfigData = { user, storage };
export type ServiceConfigData = typeof serviceConfigData;

export const serviceConfig = Registry.from<ServiceConfigData>(serviceConfigData, { consistent: true });
export const serviceListNames = [serviceConfig.get('user.name'), serviceConfig.get('storage.name')];

// Load all permission keys
export const permissionKeys: string[] = [];
loadPermissionKeys(user.permissions, permissionKeys);
loadPermissionKeys(storage.permissions, permissionKeys);
