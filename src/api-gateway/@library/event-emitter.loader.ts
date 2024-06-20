import { eventConstant } from '@shared-library';
import { EventEmitter, Is } from '@mvanvu/ujs';
import { Injectable, InjectionToken, OnApplicationBootstrap, OnApplicationShutdown } from '@nestjs/common';
import { ContextIdFactory, DiscoveryService, MetadataScanner, Reflector } from '@nestjs/core';
import { Injector } from '@nestjs/core/injector/injector';
import { InstanceWrapper } from '@nestjs/core/injector/instance-wrapper';
import { Module as ModuleWrapper } from '@nestjs/core/injector/module';

@Injectable()
export class EventEmitterLoader implements OnApplicationBootstrap, OnApplicationShutdown {
   private readonly injector = new Injector();

   constructor(
      private readonly eventEmitter: EventEmitter,
      private readonly discoveryService: DiscoveryService,
      private readonly metadataScanner: MetadataScanner,
      private readonly reflector: Reflector,
   ) {}

   onApplicationBootstrap() {
      const providers = this.discoveryService.getProviders();
      const controllers = this.discoveryService.getControllers();

      [...providers, ...controllers].forEach((wrapper: InstanceWrapper) => {
         const { instance } = wrapper;

         if (!instance || wrapper.isAlias) {
            return;
         }

         this.metadataScanner.getAllMethodNames(Object.getPrototypeOf(instance) || {}).forEach((methodName: string) => {
            const method = instance[methodName] as Function;
            const metadata = this.reflector.get<string | string[] | undefined>(eventConstant.metadataKey, method);

            if (metadata?.length) {
               const isScopeRequest = !wrapper.isDependencyTreeStatic();
               const handler = isScopeRequest
                  ? async (...args: unknown[]) => {
                       const contextId = ContextIdFactory.create();
                       const collection = new Map<InjectionToken, InstanceWrapper<any>>();
                       const loadCollection = (items: Map<InjectionToken, InstanceWrapper<any>>) =>
                          items.forEach((item, key) => collection.set(key, item));
                       const moduleRef = wrapper.host as ModuleWrapper;
                       loadCollection(moduleRef.providers);
                       loadCollection(moduleRef.controllers);
                       const inst = await this.injector.loadPerContext(
                          instance,
                          moduleRef,
                          collection,
                          contextId,
                          wrapper,
                       );
                       method.call(inst, ...args);
                    }
                  : async (...args: unknown[]) => method.call(instance, ...args);

               (Is.array(metadata) ? metadata : [metadata]).forEach((event) => this.eventEmitter.on(event, handler));
            }
         });
      });
   }

   onApplicationShutdown() {
      this.eventEmitter.remove();
   }
}
