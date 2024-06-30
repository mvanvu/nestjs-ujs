import { HttpStatus } from '@nestjs/common';
import { AuthEntity } from '@shared-library';
import { request, spec } from 'pactum';
request.setBaseUrl(`http://localhost:${process.env.PORT}/api/v1`);

export class ContainerTest {
   static run(describeTitle: string, describeCallback: (container?: ContainerTest) => any | Promise<any>): void {
      describe(describeTitle, () => describeCallback(new ContainerTest()));
   }

   signIn(username: string, password: string): Promise<AuthEntity> {
      return spec()
         .post('/users/signin')
         .withBody({ username, password })
         .expectStatus(HttpStatus.OK)
         .stores('rootAccessToken', 'data.tokens.access')
         .returns('data');
   }

   signInWithRoot(): Promise<AuthEntity> {
      return this.signIn('rainy.mi', 'MyStr0ngPassWord!');
   }
}
