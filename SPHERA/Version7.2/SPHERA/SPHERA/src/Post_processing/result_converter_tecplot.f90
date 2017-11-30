!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: result_converter_tecplot               
! Description: Post-processing for .dat (fluid dynamics parameters)     
!----------------------------------------------------------------------------------------------------------------------------------
subroutine result_converter_tec(str)
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
character(6),intent(IN) :: str
integer(4) :: npi,i,j,k,k1,k2,numcells,numpoints
double precision :: curtime
integer(4),dimension(24) :: iappo
integer(4),dimension(:),allocatable :: finger
character(len=256) :: stringa,header_string
character(len=120) :: filetec,prefix
character(len=10)  :: cargo
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
!------------------------
! Initializations
!------------------------
! .. check for time sampling is active
curtime = tempo  
!------------------------
! Statements
!------------------------
if ((curtime<val_time).and.(index(str,'fine')==0)) return
      curtime = tempo - MOD(tempo,abs(freq_time))  
   if (nag>0) then
        block = block + 1
        nblocchi = nblocchi + 1     
              if (nblocchi>maxnumblock) then
               nblocchi = maxnumblock    
            endif
   blocchi(nblocchi) = block
   Time_Block(nblocchi) = curtime
   prefix = nomecaso
   write(cargo,'(i6)') block
   cargo = adjustl(cargo)
   
  filetec =                                                                   &
  "TECConverter_"//prefix(1:len_trim(prefix))//"_block_"//cargo(1:len_trim(cargo))//".dat"

   open(unit=unittec,file=filetec,form='formatted',access='sequential',        &
      status='unknown')
   rewind(unit=unittec)
   
   numpoints = count(pg(1:nag)%cella>0)
   allocate (finger(numpoints))
   k = 0
   
   do npi=1,nag
      if (pg(npi)%cella==0) cycle
      k = k + 1
      finger(k) = npi
   enddo
    write(cargo,'(i6)') numpoints
    cargo = adjustl(trim(cargo))
   do i=1, numpoints,1
        write (unittec,'(1X,10(1x,F10.3))') pg(finger(i))%coord(1),pg(finger(i))%coord(2),pg(finger(i))%coord(3), &
                                                    !  pg(finger(i))%vel(1), pg(finger(i))%vel(2),pg(finger(i)%vel(3), &
                                                      pg(finger(i))%pres, &
                                                      pg(finger(i))%dens, &
                                                      pg(finger(i))%mass, &
                                                      pg(finger(i))%visc
  enddo
     flush(unittec)
      close (unittec)
      deallocate (finger)
!        endif
endif 
!------------------------
! Deallocations
!------------------------
return
end subroutine result_converter_tec
