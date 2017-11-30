!----------------------------------------------------------------------------------------------------------------------------------
! Program unit: Gest_Input                   
! Description: Input check and management.  �����ļ���麯��               
!----------------------------------------------------------------------------------------------------------------------------------

subroutine Gest_Input
!------------------------
! Modules
!------------------------ 
use I_O_file_module
use Static_allocation_module
use Dynamic_allocation_module
use I_O_diagnostic_module
!------------------------
! Declarations
!------------------------
implicit none
integer(4), parameter :: ner0 = 0
! Flag transformation quadrilateral face in two triangular faces 
logical :: OnlyTriangle 
integer(4) :: npi,ier,i,n,isi,nfc,nt,nrecords,IC_loop,InputErr
integer(4) :: machine_Julian_day,machine_hour,machine_minute,machine_second
integer(4),dimension(20) :: NumberEntities 
character(len=lencard) :: nomsub = "GEST_INPUT"
character(len=lencard) :: ainp,msg_err
character(10), external :: ltrim
character(80), external :: lcase
!------------------------
! Explicit interfaces
!------------------------
!------------------------
! Allocations
!------------------------
!------------------------
! Initializations
!------------------------
write(nout,'(1x,a)') ">> Input data management starts... "
write(nscr,'(1x,a)') ">> Input data management starts... " !��Ļ���
! Array deallocation
call Gest_Dealloc(nomsub) !�����г������������
! Spatial dimensionality
ncord = 0                         
NumberEntities = 0
Domain%istart = 0    
Domain%start = zero            
Domain%file = " " 
Domain%NormFix = .FALSE.         
Domain%Slip = .FALSE.
OnlyTriangle = .FALSE.
diffusione = .FALSE.
esplosione = .FALSE.
erosione = .FALSE.
Restart = .FALSE.
tempo = zero
dt = zero
it_start = 0
!------------------------
! Statements
!------------------------
if (exetype=="linux") then
   call system("date +%j%H%M%S>date_0.txt")
   open(unit_time_elapsed,file='date_0.txt',status="unknown",form="formatted")
   read(unit_time_elapsed,'(i3,i2,i2,i2)') machine_Julian_day,machine_hour,    &
      machine_minute,machine_second
   close(unit_time_elapsed)
   Domain%t0 = machine_Julian_day * 24 * 60 * 60 + machine_hour * 60 * 60 +    &
               machine_minute * 60 + machine_second
endif


! Allocations of temporary arrays
!write(nout,'(1x,a)') ">> Temporary storage allocation in routine "//trim(nomsub)
write(nout,'(1x,a)') ">> ������ʱ�洢λ�� "//trim(nomsub)
allocate(Vertice(SPACEDIM,1),BoundaryFace(1),Tratto(1),BoundaryVertex(1),      &
   stat=ier)
   if (ier/=0) then
      write(nout,'(1x,a,i2)')                                                  &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX not allocated. Error code: "&
         ,ier
   call diagnostic(arg1=4,arg3=nomsub)
   
   else
      write(nout,'(1x,a)')                                                     &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX successfully allocated "
endif
allocate(Partz(1),Med(1),Control_Sections(0:1),Control_Points(1),              &
   Control_Lines(1),stat=ier)
   if (ier/=0) then
      write(nout,'(1x,a,i2)')                                                  &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines not allocated. Error code: "&
         ,ier
      call diagnostic(arg1=4,arg3=nomsub)
      else
         write(nout,'(1x,a)')                                                  &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines successfully allocated "
      endif
!������������ļ�
! Input data reading
! To read for the first time the input file to get the parameters for 
! array sizing 
NumVertici = 1
NumFacce = 1
NumTratti = 1
NumBVertices = 1
NPartZone = 1
NMedium = 1
NSections = 1
NPoints = 1
NLines = 1

!//////////////////////////////////////////////////////////

call ReadInput(NumberEntities,OnlyTriangle,InputErr,ainp)


!/////////////////////////////////////////////////////////
! An error was detected in the input data. Execution fails.
msg_err = trim("dimensioning")
if (InputErr/=0) then
   InputErr = InputErr + 300
   call diagnostic(arg1=5,arg2=InputErr,arg3=msg_err)
endif
write(nout,'(1x,a)')                                                           &
   ">Data are read from an ASCII input file in the routine ReadInput"
write(nscr,'(1x,a)')                                                           &
">Data are read from an ASCII input file in the routine ReadInput"
! Deallocations of temporary arrays
write(nout,'(1x,a)') ">> Deallocation of temporary arrays "
deallocate(Vertice,BoundaryFace,Tratto,BoundaryVertex,stat=ier)
if (ier/=0) then
   write(nout,'(1x,a,i2)')                                                     &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX not deallocated. Error code: "&
      ,ier
      call diagnostic(arg1=4,arg3=nomsub)
      else
         write(nout,'(1x,a)')                                                  &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX successfully deallocated "
endif
deallocate(Partz,Med,Control_Sections,Control_Points,Control_Lines,stat=ier)
if (ier/=0) then
   write(nout,'(1x,a,i2)')                                                     &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines not deallocated. Error code: "&
      ,ier
   call diagnostic(arg1=4,arg3=nomsub)
   else
      write(nout,'(1x,a)')                                                     &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines successfully deallocated "
endif
! A restart procedure has been invoked: restart positioning 
! (step number / step time)
if ((Domain%istart>0).or.(Domain%start>zero)) then
   Restart = .True.
! To open the restart file from which restart data will be restored
   open(unit=nsav,file=trim(Domain%file),form="unformatted",status="old",      &
      iostat=ier)
   if (ier/=0) then
      ainp = Domain%file
      call diagnostic(arg1=5,arg2=201,arg3=trim(ainp))
      else
         write(nout,'(1x,a)')                                                  &
">Data are read from the restart file"//trim(Domain%file)//" in the routine ReadRestartFile"
         write(nscr,'(1x,a)')                                                  &
">Data are read from the restart file in the routine ReadRestartFile"
   endif
! To restore data from the restart file
   call ReadRestartFile (trim("heading"), ier, nrecords)
   msg_err = trim("heading")
   if (ier/=0) then
      ier = ier + 200
      call diagnostic(arg1=5,arg2=ier,arg3=msg_err)
   endif
   NPoints = NumberEntities(4)
   NPointsl = NumberEntities(6)
   NPointst = NumberEntities(4) + NumberEntities(6) + NumberEntities(13)
   NLines = NumberEntities(5)
   NSections = NumberEntities(12)
   NPointse = NumberEntities(13)
   else
! No restart: standard initialization
      ncord = NumberEntities(1)
      nmedium = NumberEntities(2)
      NPartZone = NumberEntities(3)
      NPoints = NumberEntities(4)
      NPointsl = NumberEntities(6)
      NPointst = NumberEntities(4) + NumberEntities(6) + NumberEntities(13)
      NLines = NumberEntities(5)
      NumVertici = NumberEntities(7)
      NumTratti = NumberEntities(8)
      NumBVertices = NumberEntities(9)
      NumBSides = NumberEntities(10)
      NumFacce = NumberEntities(11)
      if (OnlyTriangle) NumFacce = NumFacce + NumberEntities(18)
      NSections = NumberEntities(12)
      NPointse = NumberEntities(13)
      if (NumberEntities(19)==1) Domain%Slip = .TRUE.
      if (NumberEntities(20)==1) Domain%NormFix = .TRUE.
endif
! Array allocations 
write(nout,'(1x,a)') ">> Final storage allocation in routine "//trim(nomsub)
allocate(Vertice(SPACEDIM,max(1,NumVertici)),BoundaryFace(max(1,NumFacce)),    &
   Tratto(max(1,NumTratti)),BoundaryVertex(max(1,NumBVertices)),               & 
   BoundarySide(max(1,NumBSides)),stat=ier)
if (ier/=0) then
   write(nout,'(1x,a,i2)')                                                     &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX, BOUNDARYSIDE not allocated. Error code: "&
      ,ier
   call diagnostic(arg1=4,arg3=nomsub)
   else
      write(nout,'(1x,a)')                                                     &
"    Arrays VERTICE, BoundaryFace, TRATTO, BOUNDARYVERTEX, BOUNDARYSIDE successfully allocated "
endif
allocate(Partz(NPartZone),Med(NMedium),OpCount(NMedium),SpCount(NMedium),      &
   EpCount(NMedium),EpOrdGrid(NMedium),Control_Sections(0:NSections+1),        &
   Control_Points(max(1,NPointst)),Control_Lines(max(1,NLines)),               &
   Section_Points(1),stat=ier)
if (ier/=0) then
   write(nout,'(1x,a,i2)')                                                     &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines, Section_Points not allocated. Error code: "&
      ,ier
   call diagnostic(arg1=4,arg3=nomsub)
   else
      write(nout,'(1x,a)')                                                     &
"    Arrays PARTZ, MED, Control_Sections, Control_Points, Control_Lines, Section_Points successfully allocated "
endif
rewind(ninp)


! Array initializations �����ʼ��
call Init_Arrays
! No restart
if (.not.Restart) then
   call ReadInput(NumberEntities,OnlyTriangle,InputErr,ainp) !�����ʼ���ļ�
   msg_err = trim("readinput")
   if (InputErr/=0) then
      InputErr = InputErr + 300
      call diagnostic(arg1=5,arg2=InputErr,arg3=msg_err)
   endif
   
   do i=1,NMedium
      if (Med(i)%codif/=zero) diffusione = .TRUE.
      if (Med(i)%Gamma/=zero) esplosione = .TRUE.
      if ((index(Med(i)%tipo,"granular")>0)) then
         erosione = .TRUE.
         modelloerosione = Med(i)%modelloerosione
      endif
    enddo
    close(ninp)
    
    nag = 0
    if (ncord==2)then
! 10 / (7 * pigreco) *(3./2.) /(h**2)
       Domain%coefke = 0.682093d0 / squareh  
! 5 / (16 * pigreco)/(h**2)
       Domain%coefkacl = 0.099472d0 / squareh   
! Particle volume
      Domain%PVolume = Domain%dd * Domain%dd
      elseif (ncord==3)then
! 1 / pigreco/(h**3)
         Domain%coefke    = 0.477464d0 / cubich   
! 15 / (64 * pigreco)/(h**3)
         Domain%coefkacl = 0.074603d0 / cubich    
! Particle volume
         Domain%PVolume = Domain%dd * Domain%dd * Domain%dd
   endif
   Control_Sections(NSections+1)%XYZRange(1:3,1) = Domain%coord(1:3,1)
   Control_Sections(NSections+1)%XYZRange(1:3,2) = Domain%coord(1:3,2)
   
! An irregular domain is considered 
   if ((Domain%tipo=="semi").or.(Domain%tipo=="bsph")) then
      if (ncord==2) then
         call DefineBoundarySideGeometry2D
         
         
         elseif (ncord==3) then  !3D���
! To replace 4-sided geometries with 3-sided geometries
            if (OnlyTriangle) call ModifyFaces (NumberEntities)
            allocate(BFaceList(NumFacce),stat=ier) 
            if (ier/=0) then
               write(nout,'(1x,a,i2)')                                         &
                  "    Array BFACELIST not allocated. Error code: ",ier
               call diagnostic(arg1=4,arg3=nomsub)
               else
                  write(nout,'(1x,a)')                                         &
                     "    Array BFACELIST successfully allocated "
            endif
            call CompleteBoundaries3D
            call DefineBoundaryFaceGeometry3D
            allocate(BoundaryConvexEdge(1:Domain%MAXNUMCONVEXEDGES),stat=ier)
            if (ier/=0) then
               write(nout,'(1x,a,i2)')                                         &
                  "   Array BoundaryConvexEdge not allocated. Error code: ",ier
               call diagnostic(arg1=4,arg3=nomsub)
               else
                  write(nout,'(1x,a)')                                         &
                     "   Array BoundaryConvexEdge successfully allocated "
            endif
            call FindBoundaryConvexEdges3D
         endif
         
         
      nagpg = 0
      if (Granular_flows_options%ID_erosion_criterion>0) then
         Med(Granular_flows_options%ID_granular)%den0_s =                      &
            Med(Granular_flows_options%ID_granular)%den0
         Med(Granular_flows_options%ID_granular)%den0 = (1.d0 -                &
            Med(Granular_flows_options%ID_granular)%gran_vol_frac_max) *       &
            Med(Granular_flows_options%ID_main_fluid)%den0 +                   &
            Med(Granular_flows_options%ID_granular)%gran_vol_frac_max *        &
            Med(Granular_flows_options%ID_granular)%den0_s 
         Med(Granular_flows_options%ID_granular)%eps =                         &
            Med(Granular_flows_options%ID_granular)%eps *                      &
            Med(Granular_flows_options%ID_granular)%den0 /                     &
            Med(Granular_flows_options%ID_granular)%den0_s
      endif
      
      IC_loop = 1
      call GeneratePart(IC_loop)
      if (.not.(allocated(pg))) then      
! To assess the number of particles. Total number of particles is 
! allocated depending on the value "nag". 
         if (nag<100) then
! Initial domain empty (inlet section) 
            PARTICLEBUFFER = INIPARTICLEBUFFER * Domain%COEFNMAXPARTI
            else
               PARTICLEBUFFER = nag * Domain%COEFNMAXPARTI
         endif
         if (((Domain%tipo=="semi").or.(Domain%tipo=="bsph"))) then
            allocate(pg(PARTICLEBUFFER),stat=ier)
            else
               call diagnostic(arg1=10,arg2=5,arg3=nomsub)
         endif   
         if (ier/=0) then
            write(nout,'(1x,a,i2)') "    Array PG not allocated. Error code: ",&
               ier
            call diagnostic(arg1=4,arg3=nomsub)
            else
               write(nout,'(1x,a)') "    Array PG successfully allocated "
               Pg(:) = PgZero
         endif
      endif
      if (Domain%RKscheme>1) then 
         if (Domain%tipo=="semi") then 
            allocate(ts0_pg(PARTICLEBUFFER),stat=ier)
            else
               call diagnostic(arg1=10,arg2=5,arg3=nomsub)
         endif   
         if (ier/=0) then
            write(nout,'(1x,a,i2)')                                            &
               "    Array ts0_pg not allocated. Error code: ",ier
            call diagnostic(arg1=4,arg3=nomsub)
            else
               write(nout,'(1x,a)') "    Array ts0_pg successfully allocated "
               ts0_pg(:) = ts_pgZero
         endif
      endif
! The background positioning grid is generated
      call CreaGrid
! Particles are created and initialized
      IC_loop = 2
      call GeneratePart(IC_loop)
      else
         call diagnostic(arg1=10,arg2=5,arg3=nomsub)
      endif
! A restart option is active
      
   else
      if (nag<100) then
! Initial domain is empty (inlet section)
         PARTICLEBUFFER = INIPARTICLEBUFFER * Domain%COEFNMAXPARTI
         else
            PARTICLEBUFFER = nag * Domain%COEFNMAXPARTI
      endif
      if (Domain%tipo=="semi") then   
         allocate(pg(PARTICLEBUFFER),stat=ier)  
         else
            call diagnostic(arg1=10,arg2=5,arg3=nomsub)
      endif   
      if (ier/=0) then
         write(nout,'(1x,a,i2)') "    Array PG not allocated. Error code: ",ier
         call diagnostic(arg1=4,arg3=nomsub)
         else
            write(nout,'(1x,a)') "    Array PG successfully allocated "
            Pg(:) = PgZero
      endif
      if (Domain%RKscheme>1) then
         if (Domain%tipo=="semi") then   
           allocate(ts0_pg(PARTICLEBUFFER),stat=ier)  
           else
           call diagnostic(arg1=10,arg2=5,arg3=nomsub)
         endif   
         if (ier/=0) then
            write(nout,'(1x,a,i2)')                                            &
               "    Array ts0_pg not allocated. Error code: ",ier
            call diagnostic(arg1=4,arg3=nomsub)
            else
               write(nout,'(1x,a)') "    Array ts0_pg successfully allocated "
               ts0_pg(:) = ts_pgZero
         endif
      endif
      allocate(BFaceList(NumFacce),stat=ier)
      if (ier/=0) then
         write(nout,'(1x,a,i2)')                                               &
            "    Array BFACELIST not allocated. Error code: ",ier
         call diagnostic(arg1=4,arg3=nomsub)
         else
            write(nout,'(1x,a)') "    Array BFACELIST successfully allocated "
      endif
      call ReadRestartFile (trim("reading"), ier, nrecords)
      msg_err = trim("reading")
      if (ier/=0) then
         ier = ier + 200
         call diagnostic(arg1=5,arg2=ier,arg3=msg_err)
      endif
      close(nsav)
      
      call ReadInput(NumberEntities,OnlyTriangle,InputErr,ainp)
      msg_err = trim("restart reading?")
      if (InputErr/=0) then
         InputErr = InputErr + 300
         call diagnostic(arg1=5,arg2=InputErr,arg3=msg_err)
      endif
      do i=1,NMedium
         if (Med(i)%codif/=zero) diffusione = .TRUE.
         if ((index(Med(i)%tipo,"granular")>0)) then
            erosione = .TRUE.
            modelloerosione = Med(i)%modelloerosione
         endif
      enddo
! To save current time for "result_converter"
      val_time = tempo
      close(ninp)
endif
! Writing on the log file 
if (Domain%ioutopt<0) then
   write(nout,*) 
   write(nout,*) "======== PARTICLES COORDINATES =========="
   do n=1,NPartZone
      write(nout,*) 
      write(nout,"(a,i5,3x,a)") "ZONE",n,Partz(n)%label
      do npi=Partz(n)%limit(1),Partz(n)%limit(2)
         write(nout,"(i10,4f14.5)") npi,pg(npi)%coord,pg(npi)%tstop  
      enddo
   enddo
endif
! Management of body dynamics input
if (n_bodies>0) then
   call Input_Body_Dynamics
endif
! Memory allocation for the particle ordering arrays
if ((Domain%tipo=="semi").or.(Domain%tipo=="bsph")) then
   allocate(NPartOrd(PARTICLEBUFFER),Icont(grid%nmax+1),stat=ier) 
   else
      call diagnostic(arg1=10,arg2=5,arg3=nomsub)
endif    
if (ier/=0) then
   write(nout,'(1x,a,i2)')                                                     &
      "    Array NPARTORD,ICONT not allocated. Error code: ",ier
   call diagnostic(arg1=4,arg3=nomsub)
   else
      write(nout,'(1x,a)') "    Array NPARTORD,ICONT successfully allocated "
      NPartOrd(:) = 0
      Icont(:) = 0
endif
if (n_bodies>0) then
   allocate(NPartOrd_bp(n_body_part),Icont_bp(grid%nmax+1),stat=ier) 
   if (ier/=0) then
      write(nout,'(1x,a,i2)')                                                  &
         "    Arrays NPARTORD_bp and ICONT_bp not allocated. Error code: ",ier
      call diagnostic(arg1=4,arg3=nomsub)
      else
         write(nout,'(1x,a)')                                                  &
            "    Arrays NPARTORD_bp and ICONT_bp successfully allocated "
         NPartOrd_bp(:) = 0
         Icont_bp(:) = 0
   endif
endif
if (Domain%tipo=="bsph") then
   call Import_ply_surface_meshes
   call DBSPH_IC_surface_elements
   if (.not.allocated(NPartOrd_w)) then
      allocate(NPartOrd_w(DBSPH%n_w+DBSPH%n_inlet+DBSPH%n_outlet),             &
         Icont_w(grid%nmax+1),stat=ier) 
      if (ier/=0) then
         write(nout,*)                                                         &
            'Error! Allocation of NPartOrd_w or Icont_w Gest_Input failed.'           
         call diagnostic(arg1=5,arg2=340)
         else
            write(nout,'(1x,a)')                                               &
               "Arrays NPARTORD_w and ICONT_w successfully allocated."
            NPartOrd_w(:) = 0
            Icont_w(:) = 0
      endif
   endif
endif
call OrdGrid1 (nout)
if (Domain%tipo=="bsph") then
   call DBSPH_find_close_faces 
   call semi_particle_volumes
endif
! To initialize pressure and density fields
if (.not.Restart) call SubCalcPreIdro
! To assess and save the sides with condition "open" (2D) 
if (ncord==2) then     
! Searching for the sides with condition "open" 
   NumOpenSides = 0
   do isi=1,NumBSides
      if ((BoundarySide(isi)%tipo=="leve").or.(BoundarySide(isi)%tipo=="velo") &
         .or.(BoundarySide(isi)%tipo=="flow").or.                              &
         (BoundarySide(isi)%tipo=="crit").or.                                  &
         (BoundarySide(isi)%tipo=="open")) then  
         NumOpenSides = NumOpenSides + 1
         if (NumOpenSides>MAXOPENSIDES) call                                   &
            diagnostic(arg1=10,arg2=6,arg3=nomsub)
            OpenSide(NumOpenSides) = isi
      endif
   enddo
! To assess and save the faces with condition "open" (3D)
   else
! Searching for the faces with condition "open" 
      NumOpenFaces = 0
      do nfc=1,NumFacce
         nt = BoundaryFace(nfc)%stretch
         if ((Tratto(nt)%tipo=="leve").or.(Tratto(nt)%tipo=="velo").or.        &
            (Tratto(nt)%tipo=="flow").or.(Tratto(nt)%tipo=="crit").or.         &
            (Tratto(nt)%tipo=="open")) then
            NumOpenFaces = NumOpenFaces + 1
            if (NumOpenFaces>MAXOPENFACES) call                                &
               diagnostic(arg1=10,arg2=7,arg3=nomsub)
            OpenFace(NumOpenFaces) = nfc
         endif
      enddo
endif
OpCount = 0
EpCount = 0
EpOrdGrid = 0
!------------------------
! Deallocations
!------------------------
return
end subroutine Gest_Input

