import { Schema } from '@mvanvu/ujs';
export class MetadataEntity {
   @Schema.string().optional().decorate()
   title?: string;

   @Schema.string().optional().decorate()
   description?: string;

   @Schema.imageUri().optional().decorate()
   image?: string;

   @Schema.string().optional().decorate()
   author?: string;

   @Schema.string().optional().decorate()
   robot?: string;

   @Schema.string().optional().decorate()
   index?: string;
}
