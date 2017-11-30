

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: VelLaw
! Description:  To impose an input kinematics to particles.   
!----------------------------------------------------------------------------------------------------------------------------------

subroutine VelLaw (vlaw,vel,np)
!------------------------
! Modules
!------------------------ 
use Static_allocation_module
use Hybrid_allocation_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4),intent(IN) :: np
double precision,intent(IN),dimension(0:3,MAXPOINTSVLAW) :: vlaw
double precision,intent(OUT),dimension(3) :: vel
integer(4) :: n
double precision :: fra
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
!------------------------
! Initializations
!------------------------
!------------------------
! Statements
!------------------------
if (np<=1) return
do n=2,np
   if (tempo>vlaw(0,n)) cycle
   fra = (tempo-vlaw(0,n-1)) / (vlaw(0,n)-vlaw(0,n-1))   
   vel(1:3) = vlaw(1:3,n-1) +  (vlaw(1:3,n) - vlaw(1:3,n-1)) * fra
   return
end do
vel(1:3) = vlaw(1:3,np)
!------------------------
! Deallocations
!------------------------
return
end subroutine VelLaw

