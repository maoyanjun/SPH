

!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: ReadInputBoundaries                    
! Description: Reading input data for the boundary treatment scheme SA-SPH (semi-analytic approach; Di Monaco et al., 2011, EACFM).                      
!----------------------------------------------------------------------------------------------------------------------------------
!##### BOUNDARIES #####
! Input parameters for the fluid domain boundaries delimited by 
! lines(2D)/faces(3D)
! In case of DB-SPH boundary treatment scheme, the parallelepiped domain 
! (mandatory) is formally represented by a fictitious SA-SPH frontier, which  
! is only used to generate the background positioning grid.
! 1st boundary
! Boundary_name               !
! Boundary_ID                 !
! Boundary_type               ! Boundary_type = fixed(wall frontier),perimeter
                              ! (fluid reservoir),source(inlet section),open, 
                              ! tapis (not recommended)

subroutine ReadInputBoundaries(NumberEntities,Partz,Tratto,BoundaryVertex,ainp,&
   comment,nrighe,ier,ninp,nout)
!------------------------
! Modules
!------------------------ 
use Static_allocation_module
use Hybrid_allocation_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4) :: nrighe,ier,ninp,nout
integer(4),dimension(20) :: NumberEntities
integer(4),dimension(NumBVertices) :: BoundaryVertex !�洢�߽����Ϣ
! Boundary        :     1
! Type            :   fixe
! Shear coeff.    :   0.0000E+00
! Vertices List /////////////////////////
!     1    2    3    4 ////////////////////////
! Color           : 111111
character(1) :: comment
character(80) :: ainp
type (TyZone),dimension(NPartZone) :: Partz
type (TyBoundaryStretch),dimension(NumTratti) :: Tratto
integer(4) :: n,index,numv,indexi,indexf,Izona,ipointer,Medium,icolor,icord    
integer(4) :: ioerr,npointv,IC_source_type,Car_top_zone,dx_CartTopog
integer(4) :: plan_reservoir_points,nag_aux,i,i1,i2,i_point,ID_first_vertex
integer(4) :: ID_last_vertex,dam_zone_ID,dam_zone_n_vertices
double precision :: pool_value,shear,velocity,trampa,valp,flowrate,H_res
double precision,dimension(3) :: values1,values3
double precision,dimension(0:3,maxpointsvlaw) :: valuev
double precision :: plan_reservoir_pos(4,2),dam_zone_vertices(4,2)
character(len=1) :: pool_plane,bends,slip
character(len=2) :: pressu
character(len=3) :: move
character(len=4) :: tipo
character(len=6) :: token_color
character(len=8) :: label
character(len=80):: token
logical,external :: ReadCheck
integer(4),external :: ptcolorrgb
character(80), external :: lcase, GetToken
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
!------------------------
! Initializations
!------------------------
npointv = 0
values3 = zero
valp = zero
!------------------------
! Statements
!------------------------
! In case of restart, input data are not read
if (restart) then
   do while (TRIM(lcase(ainp))/="##### end boundaries #####")
      call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
      if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"BOUNDARIES DATA",ninp,nout))   &
         return
   enddo
   return
endif

!///////////////////////////////////////////////////////
call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"BOUNDARIES DATA",ninp,nout)) return
! Reading input data
do while (TRIM(lcase(ainp))/="##### end boundaries #####")
   label = ainp(1:8)
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"BOUNDARIES INDEX",ninp,nout))     &
      return
   token = GetToken(ainp,1,ioerr)
   read(token,*,iostat=ioerr) indexi !
   
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"BOUNDARY INDEX",ninp,nout)) return
   indexf = indexi
   token = GetToken(ainp,2,ioerr)
   if (token/="") read(token,*,iostat=ioerr) indexf
   NumberEntities(8) = max(indexf,NumberEntities(8))
! Boundary type
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
   if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"BOUNDARY TYPE",ninp,nout)) return
   tipo = lcase(ainp(1:4)) !�߽����ͱ��
   numv = 0
   ipointer = 0
   move = "   "
   Medium = 0
   icolor = Z'FF0000'  
   values1 = zero
   values3 = zero
   pool_plane = " "
   pool_value = zero
   shear = zero
   velocity = zero
   flowrate = zero
   trampa = zero
   pressu = "  "
   valp = zero
   IC_source_type = 0
   Car_top_zone = 0
   dx_CartTopog = 0.d0
   H_res = 0.d0
   ID_first_vertex = 0
   ID_last_vertex = 0
   plan_reservoir_points = 0
   nag_aux = 0
   dam_zone_ID = 0
   dam_zone_n_vertices = 0
   plan_reservoir_pos = 0.d0 
   dam_zone_vertices = 0.d0
   
   select case(tipo)
! Boundary condition "leve", "crit" or "open"
      case("leve", "crit", "open")    
         NumberEntities(3) = NumberEntities(3) + 1
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"FIXED: RRGGBB COLOR",ninp,  &
            nout)) return
! Boundary condition "fixe"
      case("fixe")    
! If (Boundary_type=="fixed"): start
!Shear_stress_coefficient    ! Shear_stress_coefficient=1.0(no-slip),           
                            ! 0.(free-slip)
!RGBColor                    ! 
! If (Boundary_type=="fixed"): end

         NumberEntities(3) = NumberEntities(3) + 1
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) shear
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                             &
            "FIXED: SHEAR STRESS COEFFICIENT",ninp,nout)) return
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"FIXED: RRGGBB COLOR",ninp,  &
            nout)) return
         
         
! Boundary condition "sour"
      case("sour")    
          
          !If (Boundary_type=="source"): start
          !flowrate 0.                 ! flowrate(inlet velocity * inlet area); 0.
          !pa IC_pressure              !
          !RGBColor 
          ! If (Boundary_type=="source"): end

         NumberEntities(3) = NumberEntities(3) + 1
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) Medium
         
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: MEDIUM INDEX",ninp, &
            nout)) return
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: FLOW RATE, TRAMPA ",&
            ninp,nout)) return
         token = GetToken(ainp,1,ioerr)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: FLOW RATE",ninp,    &
            nout)) return
         read(token,*,iostat=ioerr) flowrate
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: FLOW RATE",ninp,    &
            nout)) return
         token = GetToken(ainp,2,ioerr)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: TRAMPA",ninp,nout)) &
            return
         read(token,*,iostat=ioerr) trampa
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE: TRAMPA",ninp,nout)) &
            return
         if (trampa/=0) then
            write(nout,*) ' '
            write(nout,*)                                                      &
'TRAMPA in SOURCE boundary is not available. TRAMPA is setted to zero; check the VELOCITY boundary.'
            write(nout,*) ' '
            write(*,*) ' '
            write(*,*)                                                         &
'TRAMPA in SOURCE boundary is not available. TRAMPA is setted to zero;  check the VELOCITY boundary.'
            write(*,*) ' '
            trampa = zero
         endif
         
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         pressu = GetToken(ainp,1,ioerr)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE PRESSURE TYPE",ninp, &
            nout)) return
         if (pressu=="pa") then  
            token = GetToken(ainp,2,ioerr)
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE PRESSURE VALUES", &
               ninp,nout)) return
            read(token,*,iostat=ioerr) valp
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE PRESSURE VALUES", &
               ninp,nout)) return
            elseif (pressu=="qp") then
               token = GetToken(ainp,2,ioerr)
               if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE PIEZO LINE",   &
                  ninp,nout)) return
               read(token,*,iostat=ioerr) valp
               if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"SOURCE PIEZO LINE",   &
                  ninp,nout)) return
               else
                  if (nout>0) write(nout,*) "Unknown option: ",trim(ainp),     &
                     " in source boundary."
                  stop
         endif
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"FIXED: RRGGBB COLOR",ninp&
            ,nout)) return
         move   = "std"
         
         ! if (motion_type=std): start
         !motion_type IC_velocity_x IC_velocity_y IC_velocity_z slip_condition
                            ! motion_type=std; slip_condition=0.0
         ! if (motion_type=std): end

! Boundary condition "velo"
         
         
      case("velo")   
          
          !
         NumberEntities(3) = NumberEntities(3) + 1
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) velocity,trampa
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                             &
            "VELO: NORMAL VELOCITY, TRAMPA",ninp,nout)) return
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"FIXED: RRGGBB COLOR",ninp,  &
            nout)) return
         move = "std"
         pressu = "pa"
         valp = zero
! Boundary condition "flow"
         
         
      case("flow")    
         NumberEntities(3) = NumberEntities(3) + 1
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) flowrate,trampa
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"VELO: FLOW RATE, TRAMPA",   &
            ninp,nout)) return
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"FIXED: RRGGBB COLOR",ninp,  &
            nout)) return
         move = "std"
         pressu = "pa"
         valp = zero
! Boundary condition "peri"
         
       ! If (Boundary_type=="perimeter"): start
       !fluid_ID                    ! (fliud reservoir)
       !colour_pattern colour_ID    ! colour_pattern=uniform,bends; colour_ID=009EA8
  
         
        case("peri")    
         NumberEntities(3) = NumberEntities(3) + 1
         
        !����������Ϣ
         call ReadInputParticlesData(NumberEntities,Medium,icolor,bends,move,  &
            slip,npointv,valuev,values3,pressu,valp,ainp,comment,nrighe,ier,   &
            ninp,nout)
         if (ier/=0) return  
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) IC_source_type,Car_top_zone
                            ! IC_reservoir_type=1(vertices and faces),2(from 
                            ! Cartesian topography---����); Car_top_zone = boundary ID 
                            ! of underlying topography(influence only if 
                            ! IC_reservoir_type==2)

         
         
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                             &
            "IC_source_type, Car_top_zone",ninp,nout)) return
         if (IC_source_type==2) then
            call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
            if (ioerr==0) read(ainp,*,iostat=ioerr) dx_CartTopog,H_res
                            ! dx_CartTopog(spatial resolution of the Cartesian 
                            ! topography); H_res(height of the reservoir free surface) 
                          

            
            
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"dx_CartTopog,H_res",ninp,&
               nout)) return
            call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
            if (ioerr==0) read(ainp,*,iostat=ioerr)                            &
               ID_first_vertex,ID_last_vertex
            ! ! ID_first_vertex,ID_last_vertex(ID of the first and
             ! and the last vertices of the reference topography)

            
            
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                          &
               "ID_first_vertex,ID_last_vertex",ninp,nout)) return
            call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
            if (ioerr==0) read(ainp,*,iostat=ioerr) plan_reservoir_points,     &
               nag_aux
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                          &
               "plan_reservoir_points and nag_aux",ninp,nout)) return
            do i2=1,plan_reservoir_points
               call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
               if (ioerr==0) read(ainp,*,iostat=ioerr) plan_reservoir_pos(i2,1)&
                  ,plan_reservoir_pos(i2,2)
               
               
               if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                       &
                  "plan_reservoir_vertices",ninp,nout)) return
            enddo
            call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
            if (ioerr==0) read(ainp,*,iostat=ioerr) dam_zone_ID,               &
               dam_zone_n_vertices
            
            !
            
            
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                          &
               "dam_zone_ID and dam_zone_vertices",ninp,nout)) return
            if (dam_zone_ID>1) then
               do i2=1,dam_zone_n_vertices
                  call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
                  if (ioerr==0) read(ainp,*,iostat=ioerr)                      &
                     dam_zone_vertices(i2,1),dam_zone_vertices(i2,2)
                  if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"dam zone vertices",&
                     ninp,nout)) return
               enddo
            endif
         endif
! Boundary condition "tapi"
      case("tapi")    
! It returns an error if the number of vertices is not 2 (3D)
         if ((numv/=2).and.(NumberEntities(1)==3)) then 
            write(nout,'(a,i15)')                                              &
               "TAPIS boundary type: 2 vertices are requested:",numv
            ier = 103
            return
         endif
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (ioerr==0) read(ainp,*,iostat=ioerr) shear
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                             &
            "TAPIS: SHEAR STRESS COEFFICIENT",ninp,nout)) return
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"TAPIS: VELOCITY COMPONENTS",&
            ninp,nout)) return
         do n=1,NumberEntities(1)
            icord = icoordp(n,NumberEntities(1)-1)
            token = GetToken(ainp,n,ioerr)
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,                          &
               xyzlabel(icord)//" VELOCITY COMPONENT (TAPIS)",ninp,nout)) return
            read(token,*,iostat=ioerr) values1(icord)
         enddo
         call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
         token = GetToken(ainp,1,ioerr)
         token_color(1:2) = token(5:6)
         token_color(3:4) = token(3:4)
         token_color(5:6) = token(1:2) 
         read(token_color,'(Z6)',iostat=ioerr) icolor
         if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"TAPIS: RRGGBB COLOR",ninp,  &
            nout)) return
         
         
! Boundary condition "pool" (only in 3D)
      case("pool")
         if (NumberEntities(1)==3) then      
            NumberEntities(3) = NumberEntities(3) + 1
            call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
            pool_plane = GetToken(ainp,1,ioerr)
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"POOL: X/Y/Z/ PLANE LABEL"&
               ,ninp,nout)) return
            token = GetToken(ainp,2,ioerr)
            if (ioerr==0) read(token,*,iostat=ioerr) pool_value
            if (.NOT.ReadCheck(ioerr,ier,nrighe,ainp,"POOL: PLANE VALUE",ninp, &
               nout)) return
            call ReadInputParticlesData(NumberEntities,Medium,icolor,bends,    &
                                        move,slip,npointv,valuev,values3,      &
                                        pressu,valp,ainp,comment,nrighe,ier,   &
                                        ninp,nout)
            if (ier/=0) return  
         endif 
      case default
         write(nout,*) "Unrecognised BOUNDARY type: ",tipo
         ier = 101
         return
      end  select
      
      
   if (ncord>0) then
      Izona = NumberEntities(3)
      Partz(Izona)%label    = label
      Partz(Izona)%tipo     = tipo
      Partz(Izona)%Medium   = Medium
      Partz(Izona)%IC_source_type = IC_source_type
      Partz(Izona)%Car_top_zone = Car_top_zone
      if (IC_source_type==2) then
         Partz(Izona)%dx_CartTopog = dx_CartTopog
         Partz(Izona)%H_res = H_res
         Partz(Izona)%ID_first_vertex = ID_first_vertex
         Partz(Izona)%ID_last_vertex = ID_last_vertex
         Partz(Izona)%plan_reservoir_points = plan_reservoir_points
         Partz(Izona)%nag_aux = nag_aux
         Partz(Izona)%plan_reservoir_pos = plan_reservoir_pos
         Partz(Izona)%dam_zone_ID = dam_zone_ID
         Partz(Izona)%dam_zone_n_vertices = dam_zone_n_vertices
         if (dam_zone_ID>1) Partz(Izona)%dam_zone_vertices = dam_zone_vertices
      endif
      Partz(Izona)%icol = icolor
      Partz(Izona)%bend = bends
      Partz(Izona)%move = move
      Partz(Izona)%slip = slip
      if (npointv/=0) then
         Partz(Izona)%npointv = npointv
         Partz(Izona)%vlaw(icoordp(0:ncord,ncord-1),1:npointv) =               &
            valuev(0:ncord,1:npointv)
      endif
      Partz(Izona)%vel = zero
      Partz(Izona)%vel(icoordp(1:ncord,ncord-1)) = values3(1:ncord)
      Partz(Izona)%trampa = trampa
      Partz(Izona)%pressure = pressu
      Partz(Izona)%valp = valp
      Partz(Izona)%Indix(1) = indexi
      Partz(Izona)%Indix(2) = indexf
      if ((pool_plane=="X").or.(pool_plane=="x")) Partz(Izona)%ipool = 1
      if ((pool_plane=="Y").or.(pool_plane=="y")) Partz(Izona)%ipool = 2
      if ((pool_plane=="Z").or.(pool_plane=="z")) Partz(Izona)%ipool = 3
      Partz(Izona)%pool = pool_value
! Constraints 
      MULTI_INDEX_LOOP: do index=indexi, indexf       
         Tratto(index)%tipo = tipo
         if (ncord==3) then
            Tratto(index)%numvertices = numv
            Tratto(index)%inivertex = ipointer
         endif
         Tratto(index)%ShearCoeff = shear
         Tratto(index)%Medium = Medium
         Tratto(index)%velocity = values1
         Tratto(index)%NormVelocity = velocity
         Tratto(index)%FlowRate = flowrate
         Tratto(index)%trampa = trampa
         Tratto(index)%zone = Izona
         Tratto(index)%ColorCode = icolor
         if ((nout>0).and.(index==indexi)) then
            if (index>1) write(nout,*)
            if (indexf==indexi) write(nout,"(1x,a,i5,1x,a)")                   &
               "Boundary        : ",indexi  !�߽���
            if (indexf/=indexi) write(nout,"(1x,a,i5,1x,a,i5)")                &
               "Boundary        : ",indexi,"   to",indexf
            write(nout,"(1x,a,2x,a)") "Type            : ",Tratto(index)%tipo  !�߽�����
            if (tipo=="fixe") then
               write(nout,"(1x,a,1pe12.4)") "Shear coeff.    : ",              &
                  Tratto(index)%ShearCoeff
               elseif (tipo=="peri") then
                  write(nout,"(1x,a,i3,1x,a)") "Medium Index    : ",           &
                     Tratto(index)%Medium
                  elseif (tipo=="pool") then
                     write(nout,"(1x,a,i3,1x,a)") "Medium Index    : ",        &
                        Tratto(index)%Medium
                     elseif (tipo=="tapi") then
                        write(nout,"(1x,a,1pe12.4)") "Shear coeff.    : ",     &
                           Tratto(index)%ShearCoeff
                        do n=1,ncord
                           icord = icoordp(n,ncord-1)
                           write(nout,"(1x,a,a,1pe12.4)") xyzlabel(icord),     &
                              " Velocity      : ",Tratto(index)%velocity(n)
                        enddo
            endif
            if (ncord==2) then
               write(nout,"(1x,a)") "Vertices List"
               write(nout,"(1x,10i5)")                                         &
BoundaryVertex(Tratto(index)%inivertex:Tratto(index)%inivertex+Tratto(index)%numvertices-1)
               !Vertices List
               ! 1    2    3    4       
               !Tratto(index)%inivertex:1
               !Tratto(index)%inivertex+Tratto(index)%numvertices-1:1+4-1=4
               ! Vertices List
               ! 5    6    7    8    5  
               !Tratto(index)%inivertex:5
               !Tratto(index)%inivertex+Tratto(index)%numvertices-1:5+4-1=8
               write(nout,"(1x,a,z6)") "Color           : ",Tratto(index)%colorCode
            endif
         endif
         select case(tipo)
            case("fixe")
               if (ncord==3) then
                  Tratto(index)%ColorCode = icolor
                  if ((nout>0).and.(index==indexi)) write(nout,                &
                     "(1x,a,z8)") "Color           : ",                        &
                     Tratto(index)%colorCode
                  Tratto(index)%ColorCode = icolor
                  write(nout,"(1x,a,z8)") "Color           : ",                &
                     Tratto(index)%colorCode
               endif
            case("tapi")
               if (ncord==3) then 
                  Tratto(index)%ColorCode = icolor
                  if (nout>0.and.index==indexi) write(nout,"(1x,a,z8)")        &
                     "Color           : ",Tratto(index)%colorCode
                  Tratto(index)%ColorCode = icolor
                  write(nout,"(1x,a,z8)") "Color           : ",                &
                     Tratto(index)%colorCode
                  else 
                     numv = Tratto(index)%numvertices
                     if (numv/=2) then 
                        if (nout>0) write(nout,'(a,i15)')                      &
                           "TAPIS boundary type: 2 vertices are requested:",   &
                           numv
                        ier = 103
                        return
                     endif
               endif
            case("peri")
               if (ncord==2) then 
                  i1 = BoundaryVertex(Tratto(index)%inivertex)
                  i =                                                          &
BoundaryVertex(Tratto(index)%inivertex+Tratto(index)%numvertices-1)
! Error if the first and the last vertices are different 
                  if (i/=i1) then 
                     if (nout>0) write(nout,'(a,2i15)')                        &
"PERIMETER boundary type: first and last vertices are different: ",i,i1
                     ier = 102
                     return
                  endif
                  if (nout>0) then
                     write(nout,"(1x,a,i3,1x,a)")                              &
                        "Zone            : ",Izona,Partz(Izona)%label
                     write(nout,"(1x,a,i3)") "Medium Index    : ",             &
                        Partz(Izona)%Medium
                     write(nout,"(1x,a,Z6.6)") "Color           : ",           &
                        Partz(Izona)%icol
                     write(nout,"(1x,a,2x,a)") "Bends           : ",           &
                        Partz(Izona)%bend
                     write(nout,"(1x,a,2x,a)") "Movement Type   : ",           &
                        Partz(Izona)%move
                     write(nout,"(1x,a,2x,a)") "Boundary Cond.  : ",           &
                        Partz(Izona)%slip
                     if (Partz(Izona)%move=="law") then
                        write(nout,"(1x,a,i3)")                                & 
                           "Velocity Table - Number of Points: ",              &
                           Partz(Izona)%npointv
                        
                        !npointv
                        
                        do i=1,Partz(Izona)%npointv
                           write(nout,"(a,i3,1p,4(2x,a,e12.4))") " Point",i,   &
                              (xyzlabel(icoordp(n,ncord-1)),                   &
                              Partz(Izona)%vlaw(icoordp(n,ncord-1),i),n=0,     &
                              ncord)
                        enddo
                     endif
                     do n=1,ncord
                        icord = icoordp(n,ncord-1)
                        write(nout,"(1x,a,a,1pe12.4)") xyzlabel(icord),        &
                           " velocity       : ",Partz(Izona)%vel(icord) 
                     enddo
                     write(nout,"(1x,a,1pe12.4)") "Time Rampa      : ",        &
                        Partz(Izona)%trampa
                     write(nout,"(1x,a,2x,a)")    "Pressure Type   : ",        &
                        Partz(Izona)%pressure
                     write(nout,"(1x,a,1pe12.4)") "Pressure Value  : ",        &
                        Partz(Izona)%valp
                  endif       
                  else
                     Tratto(index)%ColorCode = icolor
                     if ((nout>0).and.(index==indexi)) write(nout,"(1x,a,z8)") &
                        "Color           : ",Tratto(index)%colorCode
               endif
               write(nout,"(1x,a,i12)") "IC_source_type  : ",                  &
                  Partz(Izona)%IC_source_type
               write(nout,"(1x,a,i12)") "Car_top_zone    : ",                  &
                  Partz(Izona)%Car_top_zone
               if (IC_source_type==2) then
                  write(nout,"(1x,a,1pe12.4)")    "dx_CartTopog    : ",        &
                     Partz(Izona)%dx_CartTopog
                  write(nout,"(1x,a,1pe12.4)") "H_res           : ",           &
                     Partz(Izona)%H_res  
                  write(nout,"(1x,a,i12)") "ID_first_vertex : ",               &
                     Partz(Izona)%ID_first_vertex
                  write(nout,"(1x,a,i12)") "ID_last_vertex  : ",               &
                     Partz(Izona)%ID_last_vertex
                  write(nout,"(1x,a,i12)") "plan_reservoir_points: ",          &
                     Partz(Izona)%plan_reservoir_points
                  write(nout,"(1x,a,i12)") "nag_aux         : ",               &
                     Partz(Izona)%nag_aux
                  do i_point=1,plan_reservoir_points
                     write(nout,"(1x,a,3(1pe12.4))") "plan_reservoir_pos   : " &
                        ,Partz(Izona)%plan_reservoir_pos(i_point,:)                  
                  enddo
                  write(nout,"(1x,a,i12)") "dam_zone_ID          : ",          &
                     Partz(Izona)%dam_zone_ID
                  write(nout,"(1x,a,i12)") "dam_zone_n_vertices  : ",          &
                     Partz(Izona)%dam_zone_n_vertices  
                  if (dam_zone_ID>1) then
                     do i_point=1,dam_zone_n_vertices
                        write(nout,"(1x,a,3(1pe12.4))")                        &
                           "dam_zone_vertices    : ",                          &
                           Partz(Izona)%dam_zone_vertices(i_point,:)                  
                     enddo
                  endif
               endif
            case("pool")
               Tratto(index)%ColorCode = icolor
               if ((nout>0).and.(index==indexi)) write(nout,"(1x,a,z8)")       &
                  "Color           : ",Tratto(index)%colorCode
         end select
      enddo MULTI_INDEX_LOOP
   endif
   call ReadRiga(ainp,comment,nrighe,ioerr,ninp)
enddo
!------------------------
! Deallocations
!------------------------
return
end subroutine ReadInputBoundaries

