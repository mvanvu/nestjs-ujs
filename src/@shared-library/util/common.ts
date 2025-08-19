import { Is, Registry, Transform, TransformType, Util } from '@mvanvu/ujs';
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

export const parseQueryParamSearch = <T extends string, R = T extends '' ? null : Record<string, any>>(q: T): R => {
   const keyword = q.trim();

   if (!keyword) {
      return null;
   }

   const mode = 'insensitive';
   let searchCondition: Record<string, any> = { contains: keyword, mode };

   // Check if advance search
   for (const markup of ['!', '^', '$', '~', '|']) {
      const index = keyword.indexOf(markup);
      let qValue: string | string[] = keyword.substring(markup.length);

      if (index === 0 && qValue) {
         switch (markup) {
            case '!':
               searchCondition = { not: { contains: qValue, mode } };
               break;

            case '^':
               searchCondition = { startsWith: qValue, mode };
               break;

            case '$':
               searchCondition = { endsWith: qValue, mode };
               break;

            case '~':
               searchCondition = { equals: qValue, mode };
               break;

            case '|':
               qValue = (qValue as string).split(/[\s\n]+/g).filter((qv: string) => !!qv.trim());

               if (qValue.length) {
                  searchCondition = { OR: qValue.map((qv: string) => ({ contains: qv, mode })) };
               }

               break;
         }
      }
   }

   return searchCondition as R;
};

export const parseQueryParamFilter = (field: string, query: Record<string, string>): null | Record<string, any> => {
   const parts = field.split(':');
   const regex = /\[([a-z0-9_.]+)\]/gi;
   let fieldName = <any>parts[0];
   let queryName = fieldName;

   if (fieldName.match(regex)) {
      queryName = fieldName.replace(regex, '');
      fieldName = fieldName.replace(queryName, '').replace(regex, '$1');
   }

   if (typeof query[queryName] === 'undefined') {
      return null;
   }

   const queryValue = Transform.toString(query[queryName]);
   const valueEquals = [];
   let valueArray: any[] = queryValue.split('|').map((val) => {
      val = val.trim();

      if (val[0] === '!') {
         valueEquals.push(false);

         return val.substring(1) || '';
      }

      valueEquals.push(true);

      return val;
   });

   const castAs = parts[1]?.toLowerCase().split(',') ?? undefined;

   if (castAs) {
      const typeTransform = castAs.filter((asType) => ['toNumber', 'toBoolean'].includes(asType)) as TransformType[];
      valueArray = valueArray.map((value) => Transform.clean(value, typeTransform));
   }

   if (valueArray.length) {
      const orWhere = [];
      const inValues = [];
      const notInValues = [];
      valueArray.forEach((value, index) => {
         const isEquals = valueEquals[index] === true;

         if (isEquals) {
            inValues.push(value);
         } else {
            notInValues.push(value);
         }
      });

      if (inValues.length) {
         orWhere.push(Registry.from().set(fieldName, { in: inValues }).valueOf());
      }

      if (notInValues.length) {
         orWhere.push(Registry.from().set(fieldName, { notIn: notInValues }).valueOf());
      }

      return orWhere.length > 1 ? { OR: orWhere } : orWhere[0];
   }

   return null;
};
