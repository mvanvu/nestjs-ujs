import { EntityResponse, PaginationMetaResponse, PaginationResponse } from './api-response.entity';
import { HttpCode, HttpStatus, Type, applyDecorators } from '@nestjs/common';
import { ApiExtraModels, ApiOperation, ApiResponse, getSchemaPath } from '@nestjs/swagger';

export type ApiResponseOptions = { statusCode?: HttpStatus; summary?: string };

export const ApiEntityResponse = <TEntity extends Type<any>>(entity: TEntity, options?: ApiResponseOptions) => {
   const statusCode = options?.statusCode ?? HttpStatus.OK;
   const decorators = [
      ApiExtraModels(EntityResponse, entity),
      ApiResponse({
         status: statusCode,
         schema: {
            allOf: [
               { $ref: getSchemaPath(EntityResponse) },
               {
                  properties: {
                     data: { $ref: getSchemaPath(entity) },
                  },
               },
            ],
         },
      }),
      HttpCode(statusCode),
   ];

   if (options?.summary) {
      decorators.push(ApiOperation({ summary: options.summary }));
   }

   return applyDecorators(...decorators);
};

export const ApiPaginationResponse = <TEntity extends Type<any>>(entity: TEntity, options?: ApiResponseOptions) => {
   const statusCode = options?.statusCode ?? HttpStatus.OK;
   const decorators = [
      ApiExtraModels(PaginationResponse, entity),
      ApiResponse({
         status: statusCode,
         schema: {
            allOf: [
               { $ref: getSchemaPath(PaginationResponse) },
               {
                  properties: {
                     meta: { $ref: getSchemaPath(PaginationMetaResponse) },
                     data: {
                        type: 'array',
                        items: { $ref: getSchemaPath(entity) },
                     },
                  },
               },
            ],
         },
      }),
      HttpCode(statusCode),
   ];

   if (options?.summary) {
      decorators.push(ApiOperation({ summary: options.summary }));
   }

   return applyDecorators(...decorators);
};
