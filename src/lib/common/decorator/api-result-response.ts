import { EntityResponse, PaginationResponse } from '@lib/common/entity';
import { HttpCode, HttpStatus, Type, applyDecorators } from '@nestjs/common';
import { ApiExtraModels, ApiOperation, ApiResponse, getSchemaPath } from '@nestjs/swagger';

export const ApiResultResponse = <TEntity extends Type<any>>(
   entity: () => TEntity,
   options?: { statusCode?: HttpStatus; pagination?: boolean; summary?: string },
) => {
   const statusCode = options?.statusCode ?? HttpStatus.OK;
   const pagination = options?.pagination === true;
   const decorators = [
      ApiExtraModels(
         () => (pagination ? PaginationResponse : EntityResponse),
         () => entity,
      ),
      ApiResponse({
         status: statusCode,
         schema: {
            allOf: [
               { $ref: getSchemaPath(() => (pagination ? PaginationResponse : EntityResponse)) },
               {
                  properties: {
                     data: pagination
                        ? {
                             type: 'array',
                             items: { $ref: getSchemaPath(() => entity) },
                          }
                        : { $ref: getSchemaPath(entity) },
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
