

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: ReadInputTitle                             
! Description:                        
!----------------------------------------------------------------------------------------------------------------------------------

subroutine ReadInputTitle(ainp,comment,nrighe,ier,ninp,nout)
!------------------------
! Modules
!------------------------ 
use Static_allocation_module                            
!------------------------
! Declarations
!------------------------
implicit none
integer(4) :: nrighe,ier,ninp,nout
character(1) :: comment
character(80) :: ainp
integer(4) :: n,ioerr
character(80),external :: lcase
logical,external :: ReadCheck
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
call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"TITLE DATA",ninp,nout)) return 
n = 0
do while (TRIM(lcase(ainp))/="##### end title #####" )
   n = n + 1
   if (n<=maxtit) then
      title(n) = ainp
      if ((ncord>0).AND.(nout>0)) write(nout,"(1x,a)") title(n)
   endif
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"TITLE DATA",ninp,nout)) return 
enddo
if ((ncord>0).AND.(nout>0)) write(nout,"(1x,a)") " "
!------------------------
! Deallocations
!------------------------
return
end subroutine ReadInputTitle

