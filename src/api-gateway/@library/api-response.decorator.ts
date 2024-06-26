import { EntityResponse, PaginationMeta, PaginationResponse } from './api-response.entity';
import { HttpCode, HttpStatus, Type, applyDecorators } from '@nestjs/common';
import { ApiExtraModels, ApiOperation, ApiQuery, ApiResponse, getSchemaPath } from '@nestjs/swagger';
import { i18n } from '@shared-library';

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

   decorators.push(
      ApiQuery({
         name: 'lang',
         required: false,
         enum: Object.keys(i18n).map((code) => `${code.substring(0, 2)}-${code.substring(2)}`),
      }),
   );

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
                     data: {
                        type: 'array',
                        items: { $ref: getSchemaPath(entity) },
                     },
                     meta: { $ref: getSchemaPath(PaginationMeta) },
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
