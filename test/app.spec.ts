import * as pactum from 'pactum';
import { signIn } from './user';

pactum.request.setBaseUrl(`http://localhost:${process.env.PORT}/api/v1`);

describe('NestJs UJS E2E', () => {
   describe('User', () => {
      signIn();
   });
});
