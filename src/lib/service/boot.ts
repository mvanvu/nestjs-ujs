import { BootServiceOptions } from '../type';

export class BootService<
   TPatterns extends Record<string, string>,
   TPermissions extends Record<string, Record<string, string>> | undefined = undefined,
> {
   constructor(private readonly options: BootServiceOptions<TPatterns, TPermissions>) {}

   get proxy(): string {
      return this.options.proxy;
   }

   get patterns(): TPatterns {
      return this.options.patterns;
   }

   get permissions(): TPermissions | undefined {
      return this.options.permissions;
   }
}
