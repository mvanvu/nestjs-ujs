import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CreateStaffDto, StaffEntity, UpdateStaffDto } from '@lib/microservice/order';
import { CRUDService } from '@microservice/lib';

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
