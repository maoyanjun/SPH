
! Program unit: calc_pelo              
! Description: Post-processing to write the free surface height.  ���㴦�����ɱ��� 
!----------------------------------------------------------------------------------------------------------------------------------

subroutine calc_pelo
!------------------------
! Modules
!------------------------ 
use I_O_file_module
use Static_allocation_module
use Hybrid_allocation_module
use Dynamic_allocation_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4) :: i,j,ncel,npartcel,mm,jj  
integer(4),dimension(1) :: minpos1,minpos2
double precision,dimension(3) :: ragtemp
integer(4),dimension(:),allocatable :: PartCelnum
double precision,dimension(:),allocatable :: PartCel
double precision,dimension(3,nlines) :: pelolib
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
allocate (PartCelnum(NMAXPARTJ),PartCel(NMAXPARTJ))
!------------------------
! Initializations
!------------------------
Pelolib = 0
! Loop over the lines
!------------------------
! Statements
!------------------------
do i=1,nlines
! Loop over the line points 
   POINTS_LOOP: do j=control_lines(i)%Icont(1)+1,control_lines(i)%Icont(2)
      ncel = control_points(j)%cella
      if (ncel==0) cycle
      if (Icont(ncel+1)<=Icont(ncel)) cycle
! Search for the two particles, which are the closest to the point 
! and to compute the averaged position 
      nPartCel = 0
      PartCelnum = 0
      PartCel = 99999
! Loop over the cell particles 
      do mm = Icont(ncel),Icont(ncel+1)-1  
         jj = NPartOrd(mm)
         ragtemp(1:3) = control_points(j)%coord(1:3) - pg(jj)%coord(1:3)
         npartcel = npartcel + 1
         PartCelnum(npartcel) = jj
         PartCel(npartcel) = ragtemp(1) * ragtemp(1) + ragtemp(2) * ragtemp(2) &
                             + ragtemp(3) * ragtemp(3)
      enddo
      minpos1 = minloc(PartCel,1)
      PartCel(minpos1) = 9999.
      minpos2 = minloc(PartCel,1)
      pelolib(1:3,i) = (pg(PartCelnum(minpos1(1)))%coord(1:3) +                &
                       pg(PartCelnum(minpos2(1)))%coord(1:3)) * half
   end do POINTS_LOOP
enddo
write (nplb,'(30g14.7)') tempo,pelolib
!------------------------
! Deallocations
!------------------------
deallocate (PartCelnum,PartCel)
return
end subroutine calc_pelo

