import { CACHE_MANAGER } from '@nestjs/cache-manager';
import { Inject, Injectable } from '@nestjs/common';
import { Cache } from 'cache-manager';

@Injectable()
export class CacheService {
   constructor(@Inject(CACHE_MANAGER) private cacheManager: Cache) {}

   async get<T = any>(key: string): Promise<T> {
      return this.cacheManager.get(key);
   }

   async set<T = any>(key: string, value: T, ttl?: number): Promise<this> {
      await this.cacheManager.set(key, value, ttl);

      return this;
   }

   async delete(key: string): Promise<this> {
      await this.cacheManager.del(key);

      return this;
   }

   async getKeys(): Promise<string[]> {
      const keys: string[] = [];

      this.cacheManager.stores.forEach(async (store) => {
         for await (const [key] of store.iterator(false)) {
            if (!keys.includes(key)) {
               keys.push(key);
            }
         }
      });

      return keys;
   }

   async clear(): Promise<this> {
      await Promise.allSettled(this.cacheManager.stores.map((store) => store.clear()));

      return this;
   }
}
