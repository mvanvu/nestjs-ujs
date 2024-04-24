import { metadata } from '@lib/metadata';
import { Body, PipeTransform, Type } from '@nestjs/common';
import { Payload } from '@nestjs/microservices';

export function IBody(...pipes: (Type<PipeTransform> | PipeTransform)[]): ParameterDecorator {
   return metadata.isGateway() ? Body(...pipes) : Payload(...pipes);
}
