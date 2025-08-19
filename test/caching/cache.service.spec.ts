import { Test } from '@nestjs/testing';
import { CacheModule, CacheService } from '@shared-library';

describe('CacheService', () => {
   let cacheService: CacheService;

   beforeEach(async () => {
      const moduleRef = await Test.createTestingModule({
         imports: [CacheModule],
      }).compile();

      cacheService = moduleRef.get(CacheService);
      await Promise.all([cacheService.set('key:0', 0), cacheService.set('key:1', [1])]);
   });

   describe('Get by key / Get all keys', () => {
      it('Should get a key successfully', async () => {
         expect(await cacheService.get('key:0')).toBe(0);
         expect(await cacheService.get('key:1')).toStrictEqual([1]);
      });

      it('Shoud get all the keys successfully', async () => {
         expect(await cacheService.getKeys()).toStrictEqual(['key:0', 'key:1']);
      });
   });
});
