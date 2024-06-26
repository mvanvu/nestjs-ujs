import { MessageMetaProvider } from '@shared-library';
import { Inject, Injectable, Scope } from '@nestjs/common';

@Injectable({ scope: Scope.REQUEST })
export class BaseService {
   @Inject(MessageMetaProvider) protected readonly meta: MessageMetaProvider;
}
