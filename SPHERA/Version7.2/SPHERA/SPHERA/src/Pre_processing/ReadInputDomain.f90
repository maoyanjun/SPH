

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: ReadInputDomain                       
! Description:                        
!----------------------------------------------------------------------------------------------------------------------------------
!##### DOMAIN #####
! Input parameters for spatial resolution and boundary treatment scheme
!D BC_string   ! D(spatial dimensionality)=2(2D),3(3D); BC_string(Boundary 
              ! treatment scheme)=semi(SA-SPH),dbsph(DB-SPH)
!dx h/dx r     ! The third parameter ("r") is optional and provides a white noise
              ! to the IC particle positions
!##### END DOMAIN #####

subroutine ReadInputDomain(NumberEntities,ainp,comment,nrighe,ier,ninp,nout,   &
                           nscr)
!------------------------
! Modules
!------------------------ 
use Static_allocation_module
use Hybrid_allocation_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4) :: nrighe,ier, ninp,nout,nscr
integer(4),dimension(20) :: NumberEntities
character(1) :: comment
character(80) :: ainp
integer(4) :: ioerr
double precision :: dd, trunc
character(80) :: token
logical,external :: ReadCheck
character(80),external :: lcase, GetToken
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
! In case of restart, input data are not read
if (restart) then
   do while (TRIM(lcase(ainp))/="##### end domain #####")
      call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DOMAIN DATA",ninp,nout))       &
         return
   enddo
   return
endif
call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DOMAIN DATA",ninp,nout)) return
do while (TRIM(lcase(ainp))/="##### end domain #####")
   token = lcase(GetToken(ainp,1,ioerr))
   read(token,*,iostat=ioerr) NumberEntities(1)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DOMAIN COORDINATES NUMBER",ninp,  &
      nout)) return 
   if ((ncord>0).and.(nout>0)) then
      write(nout,"(1x,a,i3,1x,a)") "Domain Dimension       : ",ncord,          &
         ncordlabel(ncord)    !计算维数
   endif
   token = lcase(GetToken(ainp,2,ioerr))
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DOMAIN TYPE",ninp,nout)) return 
   select case (token(1:4))
      case ("bsph","semi") 
         Domain%tipo = token(1:4)
         if (ncord>0.and.nout>0) then
            write(nout,"(1x,a,1x,a)"  ) "Domain Type            : ",           &
               trim(token)
         endif      
      case default
         ier = 3
         return
   end select
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
   if (ioerr==0) read(ainp,*,iostat=ioerr) dd,trunc
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DD & TRUNC",ninp,nout)) return
   Domain%dd = dd
   Domain%trunc = trunc
   token = lcase(GetToken(ainp,3,ioerr))
   if (token(1:1)=='r') then
      Domain%RandomPos = 'r'
      else
      Domain%RandomPos = 'n'
   endif
   if ((ncord>0).and.(nout>0)) then
      write(nout,"(1x,a,1pe12.4)") "Dd                     : ",dd   !粒子间距
      write(nout,"(1x,a,1pe12.4)") "Trunc                  : ",trunc !截断系数
      write(nout,"(1x,a,1x,a)") "Random Initial Position: ",Domain%RandomPos
      write(nout,"(1x,a)")  " "
   endif
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DOMAIN DATA",ninp,nout)) return
enddo
!------------------------
! Deallocations
!------------------------
return
end subroutine ReadInputDomain

