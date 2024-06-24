import { Is, Util } from '@mvanvu/ujs';
export const snackToCamelCase = <T>(data: any): T => {
   if (Is.array(data)) {
      data.forEach(snackToCamelCase);
   } else if (Is.object(data)) {
      for (const k in data) {
         const camelKey = Util.snackToCamelCase(k);

         if (camelKey !== k) {
            data[camelKey] = data[k];
            delete data[k];
         }

         if (!Is.primitive(data[camelKey])) {
            snackToCamelCase(data[camelKey]);
         }
      }
   }

   return data as T;
};
