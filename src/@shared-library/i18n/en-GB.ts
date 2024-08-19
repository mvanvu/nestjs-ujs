export default {
   // Common
   ACCESS_DENIED: `You don't have permission to access this resource`,
   FORBIDDEN: 'Access denied.',
   WRONG_CREDENTIALS: 'Credentials are wrong',
   RESULT_FOUND_0: 'Result not found',
   RESULT_FOUND_1: 'A result found',
   RESULT_FOUND_N: '%count% results found',
   ITEM_ID_NOT_FOUND: 'The item with ID(%id%) not found',
   ITEM_CREATED: 'Item created',
   ITEM_UPDATED: 'Item updated',
   ITEM_DELETED: 'Item deleted',
   EMPTY_DATA_WARN: `Data can't be empty`,
   UNIQUE_CONSTRAINT_WARN: 'The value is duplicate, please choose another',
   FIELD_REQUIRED: 'The field %field% is required',

   // User
   USER_UPDATE_SELF_STATUS_DENIED: `You can't update yourself status`,
   USER_UPDATE_SELF_GROUP_DENIED: `You can't update your group because you aren't a root user`,
   USER_UPDATE_SAME_PERMIT_DENIED: `You can't update the user who has the same permission with you`,
   USER_UPDATE_GREATER_PERMIT_DENIED: `You can't update the user who has the greater permissions than you`,
   USER_DELETE_SELF_DENIED: `You can't delete yourself`,
   USER_DELETE_SAME_PERMIT_DENIED: `You can't delete the user who has the same permission with you`,
   USER_DELETE_GREATER_PERMIT_DENIED: `You can't delete the user who has the greater permissions than you`,

   // Content
   CONTENT_CATEGORY_$ID_NOT_EXISTS: `The category ID $id doesn't exists`,
   CONTENT_$SLUG_READY_EXISTS: 'The slug[%slug%] has already exists',
   CONTENT_EMPTY_SLUG_WARN: 'The slug must not be empty',
};
