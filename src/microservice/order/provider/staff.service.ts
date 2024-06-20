import { Inject, Injectable } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { CRUDService } from '@microservice/@library';
import { CreateStaffDto, UpdateStaffDto } from '../dto';
import { StaffEntity } from '../entity';

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
