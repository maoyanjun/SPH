
!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: ReadDBSPH                    
! Description: Reading input data for the DB-SPH boundary treatment scheme (Amicarelli et al., 2013, IJNME).                   
!----------------------------------------------------------------------------------------------------------------------------------

subroutine ReadDBSPH (ainp,comment,nrighe,ier,ninp,nout)
!------------------------
! Modules
!------------------------ 
use Static_allocation_module
use Dynamic_allocation_module
use I_O_diagnostic_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4),intent(inout) :: nrighe,ier,ninp,nout
character(80),intent(inout) :: ainp 
character(1),intent(inout) :: comment
logical :: MUSCL_boundary_flag,in_built_monitors
integer(4) :: ioerr,n_monitor_points,n_monitor_regions,i,alloc_stat            
integer(4) :: dealloc_stat,n_kinematics_records,j,n_inlet,n_outlet
double precision :: dx_dxw,k_w
integer(4),allocatable,dimension(:) :: monitor_IDs
double precision,dimension(:) :: monitor_region(6)           
character(80) :: lcase
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
! In case of restart, input data are not read
if (restart) then
! Lower case letters are required
   do while (TRIM(lcase(ainp))/="##### end dbsph #####") 
      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH DATA",ninp,nout)) return
   enddo
  return
endif
call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH DATA",ninp,nout)) return
do while (TRIM(lcase(ainp))/="##### end dbsph #####")
! Reading the ratio between the fluid and the semi-particle sizes (dx/dx_w)
   read(ainp,*,iostat=ioerr) dx_dxw,MUSCL_boundary_flag,k_w
   !dx_dxw:ˮ�����Ӻͱ߽����ӵı���
   !MUSCL_boundary_flag���Ƿ���MUSCL_boundary
   !k_w:??
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH GENERAL INPUT",ninp,nout))  &
      return
   call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
   read(ainp,*,iostat=ioerr) n_monitor_points,n_monitor_regions !����X�������
   !if(n_monitor_points>0):start
   !ID_wall_element_monitor_1 ...ID_wall_element_monitor_n
   !if(n_monitor_points>0):end
   
   !if(n_monitor_regions>0):start
   !xmin,xmax,ymin,ymax,zmin,zmax   !(monitoring region vertices)
   !if(n_monitor_regions>0):end 
   
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_monitor_numbers",ninp,nout))&
      return
   if (n_monitor_points>0) then
      if (.not.allocated(monitor_IDs)) allocate (monitor_IDs(n_monitor_points),&
         STAT=alloc_stat)
      if (alloc_stat/=0) then
         write(nout,*)                                                         &
'Allocation of monitor_IDs in ReadDBSPH failed; the program terminates here'
! Stop the main program
         stop 
      endif
      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
      read(ainp,*,iostat=ioerr) monitor_IDs(:)
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_monitor_IDs",ninp,nout)) &
         return
      endif
      if (n_monitor_regions==1) then
         call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
         read(ainp,*,iostat=ioerr) monitor_region(:)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_monitor_region",ninp, &
            nout)) return
      endif
      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
      
      
      
      read(ainp,*,iostat=ioerr) n_kinematics_records,in_built_monitors
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_KINEMATICS",ninp,nout))  &
         return  
      if (.not.(allocated(DBSPH%kinematics))) then
         allocate (DBSPH%kinematics(n_kinematics_records,4),STAT=alloc_stat)
         if (alloc_stat/=0) then
            write(nout,*)                                                      &
'Error! Allocation of DBSPH%kinematics in ReadDBSPH failed; the program terminates here.'
            call diagnostic (arg1=5,arg2=340)
! Stop the main program
            stop 
            else
               write(nout,'(1x,a)')                                            &
"Array DBSPH%kinematics successfully allocated in subrouitne ReadDBSPH."
         endif
      endif  
      do j=1,n_kinematics_records
         call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
         read(ainp,*,iostat=ioerr) DBSPH%kinematics(j,:)  
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_KINEMATICS_RECORDS",  &
            ninp,nout)) return            
      enddo
      !imposed_kinematics_records flag_in-built_monitors
                                    ! imposed_kinematics_records(number of 
                                    ! records, which describe a possible imposed
                                    ! kinematics; 
      !�����˶�ʱ�䲽��
                                    ! flag_in-built_monitors(logical): flag for 
                                    ! in-built motion of control lines and 
                                    ! DB-SPH frontiers
! if (imposed_kinematics_records==1): start
!time_1 velocity_x_1 velocity_x_2 velocity_x_3
! ...                               ! other possible  records
!time_last velocity_x_last velocity_x_last velocity_x_last
                                    ! (records for the imposed translational 
                                    ! kinematics to frontiers)
!if (imposed_kinematics_records==1): end

      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
      read(ainp,*,iostat=ioerr) n_inlet,n_outlet
      !n_inlet n_outlet             ! n_inlet(number of inlet sections)
                                    ! n_outlet(number of outlet sections)

      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_INLET_OUTLET",ninp,nout))&
         return 
      if (n_inlet>0) then
         if (.not.allocated(DBSPH%inlet_sections)) then
            allocate (DBSPH%inlet_sections(n_inlet,10),STAT=alloc_stat)
            if (alloc_stat/=0) then
               write(nout,*)                                                   &
'Allocation of DBSPH%inlet_sections in ReadDBSPH failed; the program terminates here'
! Stop the main program
               stop 
         endif
      endif
   endif
   do j=1,n_inlet
      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
! Reading position, normal and velocity of an inlet surface element      
      read(ainp,*,iostat=ioerr) DBSPH%inlet_sections(j,:)  
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_INLET_SECTIONS",ninp,    &
         nout)) return            
   enddo 
 
!if (n_inlet>1): start
!x_inlet_1 y_inlet_1 z_inlet_1 n_x_inlet_1 n_y_inlet_1 n_z_inlet_1 velocity_x_inlet_1 velocity_y_inlet_1 velocity_z_inlet_1 L_inlet_1
!...                                 ! (other possible records)
!x_inlet_last y_inlet_last z_inlet_last n_x_inlet_last n_y_inlet_last n_z_inlet_last velocity_x_inlet_last velocity_y_inlet_last velocity_z_inlet_last L_inlet_last
                                    ! inlet section data: position, normal, 
                                    ! velocity, length.
! if (n_inlet>1): end

   
   
   
   if (n_outlet>0) then
! Reading position and normal of an outlet surface element       
      if (.not.allocated(DBSPH%outlet_sections)) then
         allocate (DBSPH%outlet_sections(n_outlet,8),STAT=alloc_stat)
         if (alloc_stat/=0) then
            write(nout,*)                                                      &
'Allocation of DBSPH_outlet_sections in ReadDBSPH failed; the program terminates here'
! Stop the main program
            stop 
         endif
      endif   
   endif
   do j=1,n_outlet
      call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
      read(ainp,*,iostat=ioerr) DBSPH%outlet_sections(j,:)  
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH_OUTLET_SECTIONS",ninp,   &
         nout)) return            
   enddo
   
! if (n_outlet>1): start
!x_outlet_1 y_outlet_1 z_outlet_1 n_x_outlet_1 n_y_outlet_1 n_z_outlet_1 velocity_x_outlet_1 velocity_y_outlet_1 velocity_z_outlet_1 L_outlet_1
!...                                 ! (other possible records)
!x_outlet_last y_outlet_last z_outlet_last n_x_outlet_last n_y_outlet_last n_z_outlet_last L_outlet_last p_outlet_last
                                    ! outlet section data: position, normal, 
                                    ! length, pressure
! if (n_outlet>1): end
   
! Writing the DB-SPH input parameters on the log file
   if ((ncord>0).and.(nout > 0)) then
      write(nout,"(1x,a,1p,e12.4)")                                            &
"dx/dx_w:........................",dx_dxw
      write(nout,"(1x,a,1p,l12)")                                              &
"MUSCL_boundary_flag:............",MUSCL_boundary_flag
      write(nout,"(1x,a,1p,e12.4)")                                            &
"k_w(semi-particle coefficient)..",k_w
      write(nout,"(1x,a,1p,i12)")                                              &
"n_monitor_points................",n_monitor_points       
      if (n_monitor_points>0) then
         do i=1,n_monitor_points
            write(nout,"(1x,a,1p,e12.4)")                                      &
"ID_monitor......................",monitor_IDs(i)        
         enddo    
      endif
      write(nout,"(1x,a,1p,i12)")                                              &
"n_monitor_regions...............",n_monitor_regions        
      if (n_monitor_regions==1) then
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_x_min: ..........",monitor_region(1)
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_x_max: ..........",monitor_region(2)
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_y_min: ..........",monitor_region(3)
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_y_max: ..........",monitor_region(4)
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_z_min: ..........",monitor_region(5)
         write(nout,"(1x,a,1p,g12.5)")                                         &
"monitor_region_z_max: ..........",monitor_region(6)
      endif
      write(nout,"(1x,a,1p,i12)")                                              &
"n_kinematics_records............",n_kinematics_records 
      write(nout,"(1x,a,1p,l12)")                                              &
"in-built_monitor_flag:..........",in_built_monitors      
      do i=1,n_kinematics_records
         write(nout,"(1x,a,1p,4(g12.4))")                                      &
"time(s),u(m/s),v(m/s),w(m/s):...",DBSPH%kinematics(i,:)        
      enddo 
      write(nout,"(1x,a,i12)")                                                 &
"n_inlet:........................",n_inlet
      do i=1,n_inlet
         write(nout,"(1x,a,1p,9(g12.4))")                                      &
"x(m),y(m),z(m),n_x,n_y,n_z,u(m/s),v(m/s),w(m/s),length(m): ",                 &
            DBSPH%inlet_sections(i,:)        
      enddo 
      write(nout,"(1x,a,i12)")                                                 &
"n_outlet:.......................",n_outlet
      do i=1,n_outlet
         write(nout,"(1x,a,1p,6(g12.4))")                                      &
"x(m),y(m),z(m),n_x,n_y,n_z,length(m),pres(Pa)............: ",                 &
            DBSPH%outlet_sections(i,:)        
      enddo       
      write(nout,"(1x,a)")  " "
! Assignment of the DB-SPH parameters 
      DBSPH%dx_dxw = dx_dxw
      DBSPH%MUSCL_boundary_flag = MUSCL_boundary_flag
      DBSPH%k_w = k_w
      DBSPH%n_monitor_points = n_monitor_points 
      DBSPH%n_monitor_regions = n_monitor_regions
      DBSPH%monitor_region(:) = monitor_region(:)  
      if (n_monitor_points>0) then
         if (.not.(allocated(DBSPH%monitor_IDs))) then
            allocate (DBSPH%monitor_IDs(n_monitor_points),STAT=alloc_stat)
            if (alloc_stat/=0) then
               write(nout,*)                                                   &
'Allocation of DBSPH%n_monitor_points in ReadDBSPH failed; the program terminates here.'
! Stop the main program
               stop 
            endif   
         endif       
         DBSPH%monitor_IDs(:) = monitor_IDs(:)
      endif
      DBSPH%n_kinematics_records = n_kinematics_records 
      DBSPH%in_built_monitors = in_built_monitors
      DBSPH%n_inlet = n_inlet   
      DBSPH%n_outlet = n_outlet
   endif
   if (allocated(monitor_IDs)) then
      deallocate (monitor_IDs,STAT=dealloc_stat)
      if (dealloc_stat/=0) then
         write(nout,*)                                                         &
'Deallocation of monitor_IDs in ReadDBSPH failed; the program terminates here.'
! Stop the main program
         stop 
      endif   
   endif   
   call ReadRiga (ainp,comment,nrighe,ioerr,ninp)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"DBSPH DATA",ninp,nout)) return
enddo
!------------------------
! Deallocations
!------------------------
return
end subroutine ReadDBSPH

