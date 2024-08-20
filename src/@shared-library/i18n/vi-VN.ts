export default {
   // Common
   ACCESS_DENIED: 'Bạn không có quyền truy cập vào vùng dữ liệu này',
   FORBIDDEN: 'Truy cập bị từ chối',
   WRONG_CREDENTIALS: 'Thông tin xác thực không đúng',
   RESULT_FOUND_0: 'Không tìm thấy kết quả nào',
   RESULT_FOUND_1: 'Tìm thấy một kết quả',
   RESULT_FOUND_N: 'Tìm thấy %count% kết quả',
   ITEM_ID_NOT_FOUND: 'Không tìm thấy kết quả với ID: %id%',
   ITEM_CREATED: 'Tạo mới thành công',
   ITEM_UPDATED: 'Cập nhật thành công',
   ITEM_DELETED: 'Xoá thành công',
   EMPTY_DATA_WARN: 'Dữ liệu bắt buộc không thể rỗng',
   UNIQUE_CONSTRAINT_WARN: 'Giá trị đã tồn tại, vui lòng sử dụng giá trị khác',
   FIELD_REQUIRED: 'Trường %field% là bắt buộc, không thể rỗng',

   // User
   USER_UPDATE_SELF_STATUS_DENIED: 'Không thể cập nhận trạng thái cho chính mình',
   USER_UPDATE_SELF_GROUP_DENIED: 'Chỉ có ROOT user mới có thể cập nhật nhóm cho chính mình',
   USER_UPDATE_SAME_PERMIT_DENIED: 'Không thể cập nhật cho người dùng ngang quyền với mình',
   USER_UPDATE_GREATER_PERMIT_DENIED: 'Không thể cập nhật cho người dùng có quyền cao hơn mình',
   USER_DELETE_SELF_DENIED: 'Không thể xoá chính mình',
   USER_DELETE_SAME_PERMIT_DENIED: 'Không thể xoá người dùng ngang quyền với mình',
   USER_DELETE_GREATER_PERMIT_DENIED: 'Không thể xoá người dùng có quyền cao hơn mình',

   // Content
   CONTENT_CATEGORY_$ID_NOT_EXISTS: 'Danh mục ID $id không tồn tại',
   CONTENT_$SLUG_READY_EXISTS: 'Định danh [%slug%] đã tồn tại',
   CONTENT_EMPTY_SLUG_WARN: 'Định danh slug không được rỗng',
   CONTENT_TAG_$ID_NOT_EXISTS: `Nhãn ID %id% không tồn tại`,
};
