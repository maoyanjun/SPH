

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: Memo_Ctl               
! Description: Post-processing for monitoring lines and points.       
!----------------------------------------------------------------------------------------------------------------------------------

subroutine Memo_Ctl
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
integer(4) :: i,j
character(255) :: nomefilectl
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
if (Npoints>0) then
   write(nomefilectl,"(a,a,i8.8,a)") nomecaso(1:len_trim(nomecaso)),'_',       &
      it_corrente,".cpt"
   open(ncpt,file=nomefilectl,status="unknown",form="formatted")
   write (ncpt,*) "Control points "
   write (ncpt,'(1x,2(a,10x),3(a,8x),3(a,5x),a,7x,a)') " Time","Iter",         &
      "X Coord","Y Coord","Z Coord","X Velocity","Y Velocity","Z Velocity",    &
      " Pressure","Density "
  flush(ncpt)
  do i=1,Npoints
     if (control_points(i)%cella==0) then
        write (ncpt,'(a,i10,a,3g14.7)') "control point ",i,                    &
           " is outside. Coord=",Control_Points(i)%coord(:)
        else
           write (ncpt,'(g14.7,i14,8(1x,g14.7))') tempo,it_corrente            &
              ,Control_Points(i)%coord(:),Control_Points(i)%vel(:),            &
              Control_Points(i)%pres,Control_Points(i)%dens
     endif
  enddo
  close (ncpt)
endif
! Printing monitoring line data
if (Nlines>0) then
   write(nomefilectl,"(a,a,i8.8,a)") nomecaso(1:len_trim(nomecaso)),'_',       &
      it_corrente,".cln"
   open(ncpt,file=nomefilectl,status="unknown",form="formatted")
   write (ncpt,*) "Control lines "
   write (ncpt,'(1x,2(a,10x),3(a,8x),3(a,5x),a,7x,a)') " Time","Iter",         &
      "X Coord","Y Coord","Z Coord","X Velocity","Y Velocity","Z Velocity",    &
      " Pressure","Density "
  flush(ncpt)
  do i=1,Nlines
     write (ncpt,*) "line #", i,"    Label ",Control_Lines(i)%label
     do j=Control_Lines(i)%icont(1),Control_Lines(i)%icont(2)
        if (control_points(j)%cella==0) then
           write (ncpt,'(a,i10,a,g14.7)') "control point ",j,                  &
              " is outside. Coord=",Control_Points(j)%coord(:)
           else
              write (ncpt,'(g14.7,i14,8(1x,g14.7))') tempo,it_corrente         &
                 ,Control_Points(j)%coord(:),Control_Points(j)%vel(:),         &
                 Control_Points(j)%pres,Control_Points(j)%dens
        endif
     enddo
  enddo
  close (ncpt)
endif
! Printing monitoring section data (not for the flow rate)
if (Nsections>0) then
   write(nomefilectl,"(a,a,i8.8,a)") nomecaso(1:len_trim(nomecaso)),'_',       &
      it_corrente,".csc"
   open(ncpt,file=nomefilectl,status="unknown",form="formatted")
   write (ncpt,*) "Control sections "
   write (ncpt,'(1x,2(a,10x),3(a,8x),3(a,5x),a,7x,a)') " Time","Iter",         &
      "X Coord","Y Coord","Z Coord","X Velocity","Y Velocity","Z Velocity",    &
      " Pressure","Density "
  flush(ncpt)
  do i=1,Nsections
     write (ncpt,*) "section #", i,"    Label ",Control_sections(i)%label,     &
        "    Type ",Control_sections(i)%Tipo
     do j=Control_sections(i)%icont(1),Control_sections(i)%icont(2)
        if (control_points(j)%cella==0) then
           write (ncpt,'(a,i10,a,g14.7)') "control point ",j,                  &
              " is outside. Coord=",Control_Points(j)%coord(:)
           else
              write (ncpt,'(g14.7,i14,8(1x,g14.7))') tempo,it_corrente         &
                 ,Control_Points(j)%coord(:),Control_Points(j)%vel(:)          &
                 ,Control_Points(j)%pres,Control_Points(j)%dens
        endif
     enddo
  enddo
  close (ncpt)
endif
!------------------------
! Deallocations
!------------------------
return
end subroutine Memo_Ctl

