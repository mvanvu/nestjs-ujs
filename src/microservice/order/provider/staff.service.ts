import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma/prisma.service';
import { CreateStaffDto, StaffEntity, UpdateStaffDto } from '@service/order';
import { CRUDService } from '@service/lib';

@Injectable()
export class StaffService {
   @Inject(PrismaService) private readonly prisma: PrismaService;

   createCRUDService(): CRUDService<PrismaService> {
      return this.prisma
         .createCRUDService('Staff')
         .validateDTOPipe(CreateStaffDto, UpdateStaffDto)
         .entityResponse(StaffEntity);
   }
}
