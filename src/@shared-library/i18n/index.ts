import { ObjectRecord, Path, Registry } from '@mvanvu/ujs';
import enGB from './en-GB';
import viVN from './vi-VN';

export const i18n = { enGB, viVN };

type LangPath = Path<typeof enGB>;

export class Language {
   private readonly registry: Registry<any, LangPath>;

   constructor(readonly code: string) {
      this.registry = Registry.from(i18n[code.replace(/[^a-zA-Z]/g, '')]);
   }

   _(path: LangPath): string {
      return this.registry.get<string>(path, path);
   }

   t<TPath extends LangPath>(path: TPath, data: ObjectRecord): string {
      let value = this.registry.get<string>(path, path);

      for (const k in data) {
         while (value.includes(`%${k}%`)) {
            value = value.replace(`%${k}%`, data[k]);
         }
      }

      return value;
   }
}
