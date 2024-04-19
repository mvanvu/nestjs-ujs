export const ENTITY_PROPERTIES: string = '__ENTITY_PROPERTIES__';

export type EntityConstructor<T> = new (...args: any[]) => T;

export function Property(): PropertyDecorator {
   return (target: Object, propertyKey: PropertyKey): void => {
      if (!target.hasOwnProperty(ENTITY_PROPERTIES)) {
         target[ENTITY_PROPERTIES] = [] as string[];
      }

      const props = target[ENTITY_PROPERTIES];

      if (!props.includes(propertyKey)) {
         props.push(propertyKey);
      }
   };
}
