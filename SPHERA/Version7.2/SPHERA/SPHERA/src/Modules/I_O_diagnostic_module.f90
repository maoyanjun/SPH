

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: I_O_diagnostic_module            
! Description: To provide global interfaces to the subroutine diagnostic.                     
!----------------------------------------------------------------------------------------------------------------------------------

module I_O_diagnostic_module
interface 
   subroutine diagnostic (arg1,arg2,arg3)
      use Static_allocation_module
      integer(4),intent(in) :: arg1
      integer(4),intent(in),optional :: arg2  !可选择参数
      character(LEN=lencard),intent(in),optional :: arg3 !可选择参数
   end subroutine 
end interface 
end module I_O_diagnostic_module

