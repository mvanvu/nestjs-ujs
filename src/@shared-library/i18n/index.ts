import { Path, Registry } from '@mvanvu/ujs';
import enGB from './en-GB';
import viVN from './vi-VN';

export const i18n = { enGB, viVN };

type LangPath = Path<typeof enGB>;

export class Language {
   private readonly registry: Registry<any, LangPath>;

   constructor(readonly code: string) {
      this.registry = Registry.from(i18n[code.replace(/[a-zA-Z]/g, '')]);
   }

   _(path: LangPath) {
      return this.registry.get(path, path);
   }
}
