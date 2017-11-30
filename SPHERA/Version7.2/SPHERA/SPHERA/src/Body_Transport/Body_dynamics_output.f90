!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: Body_dynamics_output
! Description: .txt output files for body transport in fluid flows. 
!----------------------------------------------------------------------------------------------------------------------------------

subroutine Body_dynamics_output
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
integer(4)       :: nbi,npi,j,nsi
double precision :: aux
! 2 auxiliary parameters: pmax_R (x>0) and pmax_L (x<0), involving boundaries 
! pointing towards positive z
double precision, allocatable, dimension(:) :: pmax_R,pmax_L
character(255) :: nomefilectl_Body_dynamics,nomefilectl_Body_particles
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
allocate(pmax_R(n_bodies))
allocate(pmax_L(n_bodies))
!------------------------
! Initializations
!------------------------
!------------------------
! Statements
!------------------------
! Loop over bodies: initialiting body maximum value of pressure (and 2 
! further parameters)
!$omp parallel do default(none) private(nbi) shared(n_bodies,body_arr,pmax_R)  &
!$omp shared(pmax_L)
do nbi=1,n_bodies
   body_arr(nbi)%pmax = -999999.
   pmax_R(nbi) = -999999.
   pmax_L(nbi) = -999999.
enddo
!$omp end parallel do
! Loop over body particles to estimate the 2 auxiliary parameters 
do npi=1,n_body_part
   if (bp_arr(npi)%area>0.) then
      body_arr(bp_arr(npi)%body)%pmax =                                        &
         max(body_arr(bp_arr(npi)%body)%pmax,bp_arr(npi)%pres)
      if (bp_arr(npi)%normal(3)>0.) then
         if (bp_arr(npi)%normal(1)>0.) then
            pmax_L(bp_arr(npi)%body) =                                         &
               max(pmax_L(bp_arr(npi)%body),bp_arr(npi)%pres)
            else
               pmax_R(bp_arr(npi)%body) =                                      &
                  max(pmax_R(bp_arr(npi)%body),bp_arr(npi)%pres)
         endif     
      endif
   endif
enddo
! File creation and heading
write(nomefilectl_Body_dynamics,"(a,a,i8.8,a)") nomecaso(1:len_trim(nomecaso)),&
   '_Body_dynamics_',it_corrente,".txt"
open (ncpt,file=nomefilectl_Body_dynamics,status="unknown",form="formatted")
if (it_corrente==1) then
   write (ncpt,*) "Body dynamics values "
   write (ncpt,'(5(7x,a),3(5x,a),3(9x,a),3(1x,a),3(8x,a),(9x,a),2(7x,a))')     &
      " Time(s)"," Body_ID"," x_CM(m)"," y_CM(m)"," z_CM(m)"," u_CM(m/s)",     &
      " v_CM(m/s)"," w_CM(m/s)"," Fx(N)"," Fy(N)"," Fz(N)", "omega_x(rad/s)",  &
      "omega_y(rad/s)","omega_z(rad/s)","Mx(N*m)","My(N*m)","Mz(N*m)",         &
      "pmax(Pa)","pmax_R(Pa)","pmax_L(Pa)"
endif
flush(ncpt)
! Loop over the bodies
do nbi=1,n_bodies
   write (ncpt,'(g14.7,1x,i14,1x,18(g14.7,1x))') tempo,nbi,                    &
      body_arr(nbi)%x_CM(:),body_arr(nbi)%u_CM(:),body_arr(nbi)%Force(:),      &
      body_arr(nbi)%omega(:),body_arr(nbi)%Moment(:),body_arr(nbi)%pmax,       &
      pmax_R(nbi),pmax_L(nbi)
enddo
close (ncpt)
! Monitoring the surface body particles
write(nomefilectl_Body_particles,"(a,a,i8.8,a)")                               &
   nomecaso(1:len_trim(nomecaso)),'_Body_particles_',it_corrente,".txt"
open (ncpt,file=nomefilectl_Body_particles,status="unknown",form="formatted")
if (it_corrente==1) then
   write (ncpt,*) " Body particle parameters"
   write (ncpt,'((7x,a),(3x,a),(7x,a),3(10x,a),3(8x,a),(6x,a),4(1x,a))')       &
      " Time(s)"," particle_ID"," body_ID"," x(m)"," y(m)"," z(m)"," u(m/s)",  &
      " v(m/s)"," w(m/s)"," p(N/m^2)"," impact_vel_body1(m/s)",                &
      " impact_vel_body2(m/s)"," impact_vel_body3(m/s)"," impact_vel_body4(m/s)"
endif
flush(ncpt)
do nsi=1,n_surf_body_part
   npi=surf_body_part(nsi)
   write (ncpt,'(g14.7,1x,2(i14,1x),11(g14.7,1x))') tempo,npi,                 &
      bp_arr(npi)%body,bp_arr(npi)%pos(:),bp_arr(npi)%vel(:),                  &
      bp_arr(npi)%pres,impact_vel(nsi,1),impact_vel(nsi,2),impact_vel(nsi,3),  &
      impact_vel(nsi,4)
enddo
!------------------------
! Deallocations
!------------------------
deallocate(pmax_R)
deallocate(pmax_L) 
return
end subroutine Body_dynamics_output

