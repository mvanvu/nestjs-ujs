import { EntityResponse, PaginationResponse } from '@lib/entity';
import { HttpCode, HttpStatus, SetMetadata, Type, applyDecorators } from '@nestjs/common';
import { ApiExtraModels, ApiOperation, ApiResponse, getSchemaPath } from '@nestjs/swagger';

export const USER_PUBLIC_KEY = 'USER_PUBLIC_KEY';
export const Public = () => SetMetadata(USER_PUBLIC_KEY, true);

export const USER_ROLE_KEY = 'USER_ROLE_KEY';
export const Permission = (options?: { key?: string; or?: string[]; and?: string[] }) =>
   SetMetadata(USER_ROLE_KEY, options ?? {});

export const ApiResultResponse = <TEntity extends Type<any>>(
   entity: TEntity,
   options?: { statusCode?: HttpStatus; pagination?: boolean; summary?: string },
) => {
   const statusCode = options?.statusCode ?? HttpStatus.OK;
   const pagination = options?.pagination === true;
   const decorators = [
      ApiExtraModels(pagination ? PaginationResponse : EntityResponse, entity),
      ApiResponse({
         status: statusCode,
         schema: {
            allOf: [
               { $ref: getSchemaPath(pagination ? PaginationResponse : EntityResponse) },
               {
                  properties: {
                     data: pagination
                        ? {
                             type: 'array',
                             items: { $ref: getSchemaPath(entity) },
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
