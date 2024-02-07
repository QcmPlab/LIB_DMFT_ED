MODULE ED_SETUP
  USE ED_INPUT_VARS
  USE ED_VARS_GLOBAL
  USE ED_AUX_FUNX
  USE ED_SECTOR
  USE SF_TIMER
  USE SF_IOTOOLS, only:free_unit,reg,file_length
  USE SF_MISC, only: assert_shape
#ifdef _MPI
  USE MPI
  USE SF_MPI
#endif
  implicit none
  private


  public :: init_ed_structure
  public :: setup_global

contains

  subroutine ed_checks_global
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG ed_checks_global: Checking input inconsistencies"
#endif
    !
    if(Lfit>Lmats)Lfit=Lmats
    if(Nspin>2)stop "ED ERROR: Nspin > 2 is currently not supported"
    if(Norb>5)stop "ED ERROR: Norb > 5 is currently not supported"
    !
    if(.not.ed_total_ud)then
       if(bath_type=="hybrid")stop "ED ERROR: ed_total_ud=F can not be used with bath_type=hybrid"
       if(bath_type=="replica".or.bath_type=="general")print*,"ED WARNING: ed_total_ud=F with bath_type=replica/general requires some care with H_bath"
       if(Norb>1.AND.(Jx/=0d0.OR.Jp/=0d0))stop "ED ERROR: ed_total_ud=F can not be used with Jx!=0 OR Jp!=0"
    endif
    !
    if(ed_mode=="superc")then
       if(Nspin>1)stop "ED ERROR: SC + Magnetism can not be solved in DMFT (ask CDMFT boys)"
       ! if(bath_type=="replica")stop "ED ERROR: ed_mode=SUPERC + bath_type=replica is not supported"
    endif
    if(ed_mode=="nonsu2")then
       if(Nspin/=2)then
          write(LOGfile,"(A)")"ED msg: ed_mode=nonSU2 with Nspin!=2 is not allowed."
          write(LOGfile,"(A)")"        spin symmetry must be imposed at bath level."
          stop
       endif
    endif

    if(Nspin>1.AND.ed_twin.eqv..true.)then
       write(LOGfile,"(A)")"WARNING: using twin_sector with Nspin>1"
    end if
    !
    if(lanc_method=="lanczos")then
       if(lanc_nstates_total>1)stop "ED ERROR: lanc_method==lanczos available only for lanc_nstates_total==1, T=0"
       if(lanc_nstates_sector>1)stop "ED ERROR: lanc_method==lanczos available only for lanc_nstates_sector==1, T=0"
    endif
    !
    if(lanc_method=="dvdson".AND.MpiStatus)then
       if(mpiSIZE>1)stop "ED ERROR: lanc_method=Dvdson + MPIsize>1: not possible at the moment"       
    endif
    !
    if(ed_diag_type=="full".AND.MpiStatus)then
       if(mpiSIZE>1)stop "ED ERROR: ed_diag_type=FULL + MPIsize>1: not possible at the moment"
    end if
    !
    if(ed_diag_type=="lanc")then
       if(ed_finite_temp)then
          if(lanc_nstates_total==1)stop "ED ERROR: ed_diag_type==lanc + ed_finite_temp=T *but* lanc_nstates_total==1 => T=0. Increase lanc_nstates_total"
       else
          if(lanc_nstates_total>1)print*, "ED WARNING: ed_diag_type==lanc + ed_finite_temp=F, T=0 *AND* lanc_nstates_total>1. re-Set lanc_nstates_total=1"
          lanc_nstates_total=1
       endif
    endif
    !
    if(ed_sectors.AND.ed_mode/="normal")then
       stop "ED_ERROR: using ed_sectors with ed_mode=[superc,nonsu2] NOT TESTED! Uncomment this line in ED_SETUP if u want to take the risk.."
    endif

    ! if(ed_mode=="superc".AND.cg_grad==0)then
    !    write(LOGfile,*)"ED_WARNING: chi2_figgf_*_superc: revert to cg_grad=1(numeric)"
    !    cg_grad=1
    ! endif

  end subroutine ed_checks_global




  !+------------------------------------------------------------------+
  !PURPOSE  : Setup Dimensions of the problem
  ! Norb    = # of impurity orbitals
  ! Nbath   = # of bath levels (depending on bath_type)
  ! Ns      = # of levels (per spin)
  ! Nlevels = 2*Ns = Total # of levels (counting spin degeneracy 2) 
  !+------------------------------------------------------------------+
  subroutine ed_setup_dimensions()
    integer :: maxtwoJz,inJz,dimJz
    integer :: isector,in,shift
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG ed_setup_dimensions: Setting up system dimensions"
#endif
    select case(bath_type)
    case default
       Ns = (Nbath+1)*Norb
    case ('hybrid')
       Ns = Nbath+Norb
       if(.not.ed_total_ud)stop "ed_setup_dimension: bath_type==hybrid AND .NOT.ed_total_ud"
    case ('replica','general')
       Ns = Norb*(Nbath+1)
    end select
    !
    select case(ed_total_ud)
    case (.true.)
       Ns_Orb = Ns
       Ns_Ud  = 1
    case (.false.)
       Ns_Orb = Ns/Norb
       Ns_Ud  = Norb
    end select
    !
    DimPh    = Nph+1
    Nlevels  = 2*Ns
    Nhel     = 1
    !
    select case(ed_mode)
    case default
       Nsectors = ((Ns_Orb+1)*(Ns_Orb+1))**Ns_Ud
    case ("superc")
       Nsectors = Nlevels+1     !sz=-Ns:Ns=2*Ns+1=Nlevels+1
    case("nonsu2")
       Nhel     = 2
       if(Jz_basis)then
          isector=0
          do in=0,Nlevels
             !algorithm to find the maximum Jz given the density
             if(in==0.or.in==2*Ns)then
                maxtwoJz=0
             else
                shift=0
                if(in<=Nbath+1)shift=Nbath-in+1
                if(in>=2*Ns-Nbath)shift=Nbath-2*Ns+in+1
                maxtwoJz = 5 + 5*Nbath - abs(in-Ns) - 2*shift
             endif
             !number of available Jz given the maximum value
             dimJz = maxtwoJz + 1
             !count of all the new Jz sectors
             do inJz=1,dimJz
                isector=isector+1
             enddo
          enddo
          Nsectors=isector
       else
          Nsectors = Nlevels+1     !n=0:2*Ns=2*Ns+1=Nlevels+1
       endif
    end select

  end subroutine ed_setup_dimensions





  

  !+------------------------------------------------------------------+
  !PURPOSE  : Init ED structure and calculation
  !+------------------------------------------------------------------+
  subroutine init_ed_structure()
    logical                          :: control
    integer                          :: i,iud
    integer                          :: dim_sector_max,iorb,jorb,ispin,jspin
    integer,dimension(:),allocatable :: DimUps,DimDws
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG init_ed_structure: Massive allocation of internal memory"
#endif
    call ed_checks_global
    !
    call ed_setup_dimensions
    !
    !
    select case(ed_mode)
    case default
       allocate(DimUps(Ns_Ud))
       allocate(DimDws(Ns_Ud))
       do iud=1,Ns_Ud
          DimUps(iud) = get_normal_sector_dimension(Ns_Orb,Ns_Orb/2)
          DimDws(iud) = get_normal_sector_dimension(Ns_Orb,Ns_Orb-Ns_Orb/2)
       enddo
    case ("superc")
       dim_sector_max=get_superc_sector_dimension(0)
    case("nonsu2")
       dim_sector_max=get_nonsu2_sector_dimension(Ns)
    end select
    !
    if(MpiMaster)then
       write(LOGfile,"(A)")"Summary:"
       write(LOGfile,"(A)")"--------------------------------------------"
       write(LOGfile,"(A,I15)")'# of levels/spin      = ',Ns
       write(LOGfile,"(A,I15)")'Total size            = ',2*Ns
       write(LOGfile,"(A,I15)")'# of impurities       = ',Norb
       write(LOGfile,"(A,I15)")'# of bath/impurity    = ',Nbath
       write(LOGfile,"(A,I15)")'# of Bath levels/spin = ',Ns-Norb
       write(LOGfile,"(A,I15)")'# of  sectors         = ',Nsectors
       write(LOGfile,"(A,I15)")'Ns_Orb                = ',Ns_Orb
       write(LOGfile,"(A,I15)")'Ns_Ud                 = ',Ns_Ud
       write(LOGfile,"(A,I15)")'Nph                   = ',Nph
       select case(ed_mode)
       case default
          write(LOGfile,"(A,"//str(Ns_Ud)//"I8,2X,"//str(Ns_Ud)//"I8,I8,I20)")&
               'Largest Sector(s)     = ',DimUps,DimDws,DimPh,product(DimUps)*product(DimDws)*DimPh
       case("superc","nonsu2")
          write(LOGfile,"(A,I15)")'Largest Sector(s)    = ',dim_sector_max
       end select
       write(LOGfile,"(A)")"--------------------------------------------"
    endif
    !
    allocate(spH0ups(Ns_Ud))
    allocate(spH0dws(Ns_Ud))
    !
    !Allocate indexing arrays
    if(Jz_basis)then
       allocate(getCsector(Norb,Nspin,Nsectors));  getCsector=-1
       allocate(getCDGsector(Norb,Nspin,Nsectors));getCDGsector=-1
    else
       allocate(getCsector(Ns_Ud,2,Nsectors))  ;getCsector  =0
       allocate(getCDGsector(Ns_Ud,2,Nsectors));getCDGsector=0
    endif
    !
    select case(ed_mode)
    case default
       allocate(getSector(0,0))
    case ("superc")
       allocate(getSector(-Ns:Ns,1))
    case ("nonsu2")
       if(Jz_basis)then
          allocate(getSector(0:Nlevels,-Nlevels:Nlevels));getSector=0
       else
          allocate(getSector(0:Nlevels,1));getSector=0
       endif
    end select
    getSector=0
    !
    allocate(getDim(Nsectors));getDim=0
    allocate(getSz(Nsectors));getSz=0
    allocate(getN(Nsectors));getN=0
    allocate(gettwoJz(Nsectors));gettwoJz=0
    allocate(getmaxtwoJz(0:Nlevels));getmaxtwoJz=0
    !
    allocate(getBathStride(Norb,Nbath));getBathStride=0
    allocate(twin_mask(Nsectors))
    allocate(sectors_mask(Nsectors))
    allocate(neigen_sector(Nsectors))
    !
    !
    finiteT = ed_finite_temp
    !
    if(ed_diag_type=="lanc")then
       if(finiteT)then
          if(mod(lanc_nstates_sector,2)/=0)then
             lanc_nstates_sector=lanc_nstates_sector+1
             write(LOGfile,"(A,I10)")"Increased Lanc_nstates_sector:",lanc_nstates_sector
          endif
          if(mod(lanc_nstates_total,2)/=0)then
             lanc_nstates_total=lanc_nstates_total+1
             write(LOGfile,"(A,I10)")"Increased Lanc_nstates_total:",lanc_nstates_total
          endif
          write(LOGfile,"(A,I3)")"Nstates x Sector = ", lanc_nstates_sector
          write(LOGfile,"(A,I3)")"Nstates   Total  = ", lanc_nstates_total
          !
          write(LOGfile,"(A)")"Lanczos FINITE temperature calculation:"
       else
          write(LOGfile,"(A)")"Lanczos ZERO temperature calculation:"
       endif
    else
       if(finiteT)then
          write(LOGfile,"(A)")"Full ED finite T calculation"
       else
          ed_diag_type='lanc'
          lanc_nstates_total=1
          lanc_dim_threshold=product(DimUps)*product(DimDws)*Dimph
          write(LOGfile,"(A)")"Full ED T=0 calculation. Set LANC_DIM_THRESHOLD to "//str(lanc_dim_threshold)
          if(lanc_dim_threshold>2**13)stop "Full ED T=0: LANC_DIM_THRESHOLD > 2**13=8192!"
       endif
    endif
    !
    Jhflag=.FALSE.
    if(Norb>1.AND.(Jx/=0d0.OR.Jp/=0d0))Jhflag=.TRUE.
    !
    !
    offdiag_gf_flag=ed_solve_offdiag_gf
    if(bath_type/="normal")offdiag_gf_flag=.true.
    if(.not.ed_total_ud.AND.offdiag_gf_flag)then
       write(LOGfile,"(A)")"ED WARNING: can not do offdiag_gf_flag=T.AND.ed_total_ud=F. Set to F."
       offdiag_gf_flag=.false.
    endif
    !
    !
    if(nread/=0.d0)then
       i=abs(floor(log10(abs(nerr)))) !modulus of the order of magnitude of nerror
       niter=nloop/3
    endif
    !
    !ALLOCATE impHloc
    if(.not.allocated(impHloc))then
       allocate(impHloc(Nspin,Nspin,Norb,Norb))
       impHloc=zero
    else
       call assert_shape(impHloc,[Nspin,Nspin,Norb,Norb],"init_ed_structure","impHloc")
    endif
    !
    !allocate functions
    allocate(impSmats(Nspin,Nspin,Norb,Norb,Lmats))
    allocate(impSreal(Nspin,Nspin,Norb,Norb,Lreal))
    allocate(impSAmats(Nspin,Nspin,Norb,Norb,Lmats)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    allocate(impSAreal(Nspin,Nspin,Norb,Norb,Lreal)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    impSmats=zero
    impSreal=zero
    impSAmats=zero
    impSAreal=zero
    !
    allocate(impGmats(Nspin,Nspin,Norb,Norb,Lmats))
    allocate(impGreal(Nspin,Nspin,Norb,Norb,Lreal))
    allocate(impFmats(Nspin,Nspin,Norb,Norb,Lmats)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    allocate(impFreal(Nspin,Nspin,Norb,Norb,Lreal)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    impGmats=zero
    impGreal=zero
    impFmats=zero
    impFreal=zero
    !
    allocate(impG0mats(Nspin,Nspin,Norb,Norb,Lmats))
    allocate(impG0real(Nspin,Nspin,Norb,Norb,Lreal))
    allocate(impF0mats(Nspin,Nspin,Norb,Norb,Lmats)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    allocate(impF0real(Nspin,Nspin,Norb,Norb,Lreal)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    impG0mats=zero
    impG0real=zero
    impF0mats=zero
    impF0real=zero
    !
    allocate(impDmats_ph(0:Lmats))
    allocate(impDreal_ph(Lreal))
    impDmats_ph=zero
    impDreal_ph=zero
    !
    allocate(impGmatrix(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb))
    !
    !allocate observables
    allocate(ed_dens(Norb),ed_docc(Norb),ed_phisc(Norb),ed_dens_up(Norb),ed_dens_dw(Norb))
    allocate(ed_mag(3,Norb))
    ed_dens=0d0
    ed_docc=0d0
    ed_phisc=0d0
    ed_dens_up=0d0
    ed_dens_dw=0d0
    ed_mag=0d0
    !
    allocate(spinChi_tau(Norb,Norb,0:Ltau))
    allocate(spinChi_w(Norb,Norb,Lreal))
    allocate(spinChi_iv(Norb,Norb,0:Lmats))
    allocate(spinChiMatrix(Norb,Norb))
    !
    allocate(densChi_tau(Norb,Norb,0:Ltau))
    allocate(densChi_w(Norb,Norb,Lreal))
    allocate(densChi_iv(Norb,Norb,0:Lmats))
    allocate(densChiMatrix(Norb,Norb))
    !
    allocate(pairChi_tau(Norb,Norb,0:Ltau))
    allocate(pairChi_w(Norb,Norb,Lreal))
    allocate(pairChi_iv(Norb,Norb,0:Lmats))
    allocate(pairChiMatrix(Norb,Norb))
    !
    allocate(exctChi_tau(0:2,Norb,Norb,0:Ltau))
    allocate(exctChi_w(0:2,Norb,Norb,Lreal))
    allocate(exctChi_iv(0:2,Norb,Norb,0:Lmats))
    allocate(exctChiMatrix(0:2,Norb,Norb))
    !
    allocate(spin_field(Norb,3))
    spin_field(:,1) = spin_field_x(1:Norb)
    spin_field(:,2) = spin_field_y(1:Norb)
    spin_field(:,3) = spin_field_z(1:Norb)
    !
  end subroutine init_ed_structure








 



  !+------------------------------------------------------------------+
  !PURPOSE: SETUP THE GLOBAL POINTERS FOR THE ED CALCULAIONS.
  !+------------------------------------------------------------------+
  subroutine setup_global
    select case(ed_mode)
    case default
       call setup_global_normal()
    case ("superc")
       call setup_global_superc()
    case("nonsu2")
       call setup_global_nonsu2()
    end select
  end subroutine setup_global



  subroutine setup_global_normal
    integer                          :: DimUp,DimDw
    integer                          :: DimUps(Ns_Ud),DimDws(Ns_Ud)
    integer                          :: Indices(2*Ns_Ud),Jndices(2*Ns_Ud)
    integer                          :: Nups(Ns_ud),Ndws(Ns_ud)
    integer                          :: Jups(Ns_ud),Jdws(Ns_ud)
    integer                          :: i,iud,iorb
    integer                          :: isector,jsector,gsector,ksector,lsector
    integer                          :: unit,status,istate,ishift,isign
    logical                          :: IOfile
    integer                          :: list_len
    integer,dimension(:),allocatable :: list_sector
    type(sector) :: sectorI,sectorJ,sectorK,sectorG,sectorL
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG setup_global_normal"
#endif
    !
    !Store full dimension of the sectors:
    do isector=1,Nsectors
       call get_DimUp(isector,DimUps)
       call get_DimDw(isector,DimDws)
       DimUp = product(DimUps)
       DimDw = product(DimDws)  
       getDim(isector)  = DimUp*DimDw*DimPh
    enddo
    !
    !
    inquire(file="state_list"//reg(ed_file_suffix)//".restart",exist=IOfile)
    if(IOfile)then
       write(LOGfile,"(A)")"Restarting from a state_list file:"
       list_len=file_length("state_list"//reg(ed_file_suffix)//".restart")
       allocate(list_sector(list_len))
       !
       open(free_unit(unit),file="state_list"//reg(ed_file_suffix)//".restart",status="old")
       status=0
       do while(status>=0)
          read(unit,*,iostat=status)istate,isector,indices
          list_sector(istate)=isector
          call get_Nup(isector,Nups)
          call get_Ndw(isector,Ndws)
          if(any(Indices /= [Nups,Ndws]))&
               stop "setup_global error: nups!=nups(isector).OR.ndws!=ndws(isector)"
       enddo
       close(unit)
       !
       lanc_nstates_total = list_len
       do isector=1,Nsectors
          neigen_sector(isector) = max(1,count(list_sector==isector))
       enddo
    else
       do isector=1,Nsectors
          neigen_sector(isector) = min(getDim(isector),lanc_nstates_sector) !init every sector to required eigenstates
       enddo
    endif
    !
    twin_mask=.true.
    if(ed_twin)then
       do isector=1,Nsectors
          call get_Nup(isector,Nups)
          call get_Ndw(isector,Ndws)
          if(any(Nups .ne. Ndws))then
             call get_Sector([Ndws,Nups],Ns_Orb,jsector)
             if (twin_mask(jsector))twin_mask(isector)=.false.
          endif
       enddo
       write(LOGfile,"(A,I6,A,I9)")"Looking into ",count(twin_mask)," sectors out of ",Nsectors
    endif
    !
    select case(bath_type)
    case default
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = Norb + (iorb-1)*Nbath + i
          enddo
       enddo
    case ('hybrid')
       do i=1,Nbath
          getBathStride(:,i)       = Norb + i
       enddo
    case ('replica','general')
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = iorb + i*Norb 
          enddo
       enddo
    end select
    !
    getCsector  = 0
    getCDGsector= 0
    do isector=1,Nsectors
       call get_Nup(isector,Nups)
       call get_Ndw(isector,Ndws)
       !
       !UPs:
       !DEL:
       do iud=1,Ns_Ud
          Jups=Nups
          Jdws=Ndws 
          Jups(iud)=Jups(iud)-1; if(Jups(iud) < 0)cycle
          call get_Sector([Jups,Jdws],Ns_Orb,jsector)
          getCsector(iud,1,isector)=jsector
       enddo
       !ADD
       do iud=1,Ns_Ud
          Jups=Nups
          Jdws=Ndws
          Jups(iud)=Jups(iud)+1; if(Jups(iud) > Ns_Orb)cycle
          call get_Sector([Jups,Jdws],Ns_Orb,jsector)
          getCDGsector(iud,1,isector)=jsector
       enddo
       !
       !DWs:
       !DEL
       do iud=1,Ns_Ud
          Jups=Nups
          Jdws=Ndws 
          Jdws(iud)=Jdws(iud)-1; if(Jdws(iud) < 0)cycle
          call get_Sector([Jups,Jdws],Ns_Orb,jsector)
          getCsector(iud,2,isector)=jsector
       enddo
       !DEL
       do iud=1,Ns_Ud
          Jups=Nups
          Jdws=Ndws 
          Jdws(iud)=Jdws(iud)+1; if(Jdws(iud) > Ns_Orb)cycle
          call get_Sector([Jups,Jdws],Ns_Orb,jsector)
          getCDGsector(iud,2,isector)=jsector
       enddo
    enddo
    return
  end subroutine setup_global_normal




  !SUPERCONDUCTING
  subroutine setup_global_superc
    integer                                           :: i,isz,in,dim,isector,jsector
    integer                                           :: sz,iorb,jsz
    integer                                           :: unit,status,istate
    logical                                           :: IOfile
    integer                                           :: anint
    real(8)                                           :: adouble
    integer                                           :: list_len
    integer,dimension(:),allocatable                  :: list_sector
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG setup_global_superc"
#endif
    isector=0
    do isz=-Ns,Ns
       sz=abs(isz)
       isector=isector+1
       getSector(isz,1)=isector
       getSz(isector)=isz
       dim = get_superc_sector_dimension(isz)
       getDim(isector)=dim*DimPh
    enddo
    !
    !
    inquire(file="state_list"//reg(ed_file_suffix)//".restart",exist=IOfile)
    if(IOfile)then
       list_len=file_length("state_list"//reg(ed_file_suffix)//".restart")
       allocate(list_sector(list_len))
       !
       open(free_unit(unit),file="state_list"//reg(ed_file_suffix)//".restart",status="old")
       read(unit,*)!read comment line
       status=0
       do while(status>=0)
          read(unit,*,iostat=status) istate,adouble,adouble,sz,isector,anint
          list_sector(istate)=isector
          if(sz/=getsz(isector))stop "setup_pointers_superc error: sz!=getsz(isector)."
       enddo
       ! status=0
       ! do while(status>=0)
       !    read(unit,*,iostat=status)istate,sz,isector
       !    list_sector(istate)=isector
       !    if(sz/=getsz(isector))stop "setup_pointers_superc error: sz!=getsz(isector)."
       ! enddo
       close(unit)
       !
       lanc_nstates_total = list_len
       do isector=1,Nsectors
          neigen_sector(isector) = max(1,count(list_sector==isector))
       enddo
    else
       do isector=1,Nsectors
          neigen_sector(isector) = min(getdim(isector),lanc_nstates_sector)
       enddo
    endif
    twin_mask=.true.
    if(ed_twin)then
       write(LOGfile,*)"USE WITH CAUTION: TWIN STATES IN SC CHANNEL!!"
       do isector=1,Nsectors
          sz=getsz(isector)
          if(sz>0)twin_mask(isector)=.false.
       enddo
       write(LOGfile,"(A,I4,A,I4)")"Looking into ",count(twin_mask)," sectors out of ",Nsectors
    endif
    !
    select case(bath_type)
    case default
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = Norb + (iorb-1)*Nbath + i
          enddo
       enddo
    case ('hybrid')
       do i=1,Nbath
          getBathStride(:,i)      = Norb + i
       enddo
    case ('replica','general')
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = Norb + (i-1)*Norb + iorb !iorb + i*Norb see above normal case
          enddo
       enddo
    end select
    !    
    getCsector=0
    !c_up
    do isector=1,Nsectors
       isz=getsz(isector);if(isz==-Ns)cycle
       jsz=isz-1
       jsector=getsector(jsz,1)
       getCsector(1,1,isector)=jsector
    enddo
    !c_dw
    do isector=1,Nsectors
       isz=getsz(isector);if(isz==Ns)cycle
       jsz=isz+1
       jsector=getsector(jsz,1)
       getCsector(1,2,isector)=jsector
    enddo
    !
    getCDGsector=0
    !cdg_up
    do isector=1,Nsectors
       isz=getsz(isector);if(isz==Ns)cycle
       jsz=isz+1
       jsector=getsector(jsz,1)
       getCDGsector(1,1,isector)=jsector
    enddo
    !cdg_dw
    do isector=1,Nsectors
       isz=getsz(isector);if(isz==-Ns)cycle
       jsz=isz-1
       jsector=getsector(jsz,1)
       getCDGsector(1,2,isector)=jsector
    enddo
  end subroutine setup_global_superc



  !NON SU(2)
  subroutine setup_global_nonsu2
    integer                                           :: i,dim,isector,jsector
    integer                                           :: in,jn,iorb,ispin
    integer                                           :: unit,status,istate
    logical                                           :: IOfile
    integer                                           :: anint
    real(8)                                           :: adouble
    integer                                           :: list_len
    integer,dimension(:),allocatable                  :: list_sector
    integer                                           :: maxtwoJz,twoJz
    integer                                           :: dimJz,inJz,shift
    integer                                           :: twoJz_add,twoJz_del,twoJz_trgt
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG setup_global_nonsu2"
#endif
    isector=0
    if(Jz_basis)then
       !pointers definition
       do in=0,Nlevels
          !
          !algorithm to find the maximum Jz given the density
          if(in==0.or.in==2*Ns)then
             maxtwoJz=0
          else
             shift=0
             if(in<=Nbath+1)shift=Nbath-in+1
             if(in>=2*Ns-Nbath)shift=Nbath-2*Ns+in+1
             maxtwoJz = 5 + 5*Nbath - abs(in-Ns) - 2*shift
          endif
          !
          !number of available Jz given the maximum value
          dimJz = maxtwoJz + 1
          !
          do inJz=1,dimJz
             if(in==0.or.in==2*Ns)then
                twoJz=0
             else
                twoJz = - maxtwoJz + 2*(inJz-1)
             endif
             isector=isector+1
             getN(isector)=in
             gettwoJz(isector)=twoJz
             getmaxtwoJz(in)=maxtwoJz
             getSector(in,twoJz)=isector
             dim = get_nonsu2_sector_dimension_Jz(in,twoJz)
             getDim(isector)=dim*DimPh
             neigen_sector(isector) = min(dim,lanc_nstates_sector)
          enddo
       enddo
    else
       do in=0,Nlevels
          isector=isector+1
          getSector(in,1)=isector
          getN(isector)=in
          dim = get_nonsu2_sector_dimension(in)
          getDim(isector)=dim*DimPh
          neigen_sector(isector) = min(dim,lanc_nstates_sector)
       enddo
    endif
    !
    !
    inquire(file="state_list"//reg(ed_file_suffix)//".restart",exist=IOfile)
    if(IOfile)then
       list_len=file_length("state_list"//reg(ed_file_suffix)//".restart")
       allocate(list_sector(list_len))
       !
       open(free_unit(unit),file="state_list"//reg(ed_file_suffix)//".restart",status="old")
       ! status=0
       ! do while(status>=0)
       !    read(unit,*,iostat=status) istate,in,isector
       !    list_sector(istate)=isector
       !    if(in/=getn(isector))stop "setup_pointers_superc error: n!=getn(isector)."
       ! enddo
       read(unit,*)!read comment line
       status=0
       do while(status>=0)
          read(unit,*,iostat=status) istate,adouble,adouble,in,isector,anint
          list_sector(istate)=isector
          if(in/=getn(isector))stop "setup_pointers_superc error: n!=getn(isector)."
       enddo
       close(unit)
       !
       lanc_nstates_total = list_len
       do isector=1,Nsectors
          neigen_sector(isector) = max(1,count(list_sector==isector))
       enddo
    else
       do isector=1,Nsectors
          neigen_sector(isector) = min(getdim(isector),lanc_nstates_sector)   !init every sector to required eigenstates
       enddo
    endif
    !
    twin_mask=.true.
    if(ed_twin)then
       write(LOGfile,*)"TWIN STATES IN nonSU2 CHANNEL: NOT TESTED!!"
       do isector=1,Nsectors
          call get_Ntot(isector,in)
          if(in>Ns)twin_mask(isector)=.false.
       enddo
       write(LOGfile,"(A,I4,A,I4)")"Looking into ",count(twin_mask)," sectors out of ",Nsectors
    endif
    !
    select case(bath_type)
    case default
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = Norb + (iorb-1)*Nbath + i
          enddo
       enddo
    case ('hybrid')
       do i=1,Nbath
          getBathStride(:,i)      = Norb + i
       enddo
    case ('replica','general')
       do i=1,Nbath
          do iorb=1,Norb
             getBathStride(iorb,i) = Norb + (i-1)*Norb + iorb
          enddo
       enddo
    end select
    !
    getCsector=0
    !c_{up,dw}
    do isector=1,Nsectors
       in=getn(isector);if(in==0)cycle
       jn=in-1
       jsector=getsector(jn,1)
       getCsector(1,1,isector)=jsector
       getCsector(1,2,isector)=jsector
    enddo
    !
    getCDGsector=0
    !cdg_{up,dw}
    do isector=1,Nsectors
       in=getn(isector);if(in==Nlevels)cycle
       jn=in+1
       jsector=getsector(jn,1)
       getCDGsector(1,1,isector)=jsector
       getCDGsector(1,2,isector)=jsector
    enddo
    if(Jz_basis)then
       !
       getCsector=-1
       !c_{Lz,Sz}
       do isector=1,Nsectors
          in=getn(isector);if(in==0)cycle
          jn=in-1
          !
          twoJz=gettwoJz(isector)
          do iorb=1,Norb
             do ispin=1,Nspin
                twoJz_del  = 2 * Lzdiag(iorb) + Szdiag(ispin)
                twoJz_trgt = twoJz - twoJz_del
                if(abs(twoJz_trgt) > getmaxtwoJz(jn)) cycle
                jsector=getSector(jn,twoJz_trgt)
                getCsector(iorb,ispin,isector)=jsector
             enddo
          enddo
       enddo
       !
       getCDGsector=-1
       !cdg_{Lz,Sz}
       do isector=1,Nsectors
          in=getn(isector);if(in==Nlevels)cycle
          jn=in+1
          !
          twoJz=gettwoJz(isector)
          do iorb=1,Norb
             do ispin=1,Nspin
                twoJz_add  = 2 * Lzdiag(iorb) + Szdiag(ispin)
                twoJz_trgt = twoJz + twoJz_add
                if(abs(twoJz_trgt) > getmaxtwoJz(jn)) cycle
                jsector=getSector(jn,twoJz_trgt)
                getCDGsector(iorb,ispin,isector)=jsector
             enddo
          enddo
       enddo
       !
    endif
  end subroutine setup_global_nonsu2








  !##################################################################
  !##################################################################
  !SECTOR PROCEDURES - Sectors,Nup,Ndw,DimUp,DimDw,...
  !##################################################################
  !##################################################################
  elemental function get_normal_sector_dimension(n,np) result(dim)
    integer,intent(in) :: n,np
    integer            :: dim
    dim = binomial(n,np)
  end function get_normal_sector_dimension
  !SUPERC
  function get_superc_sector_dimension(mz) result(dim)
    integer :: mz
    integer :: i,dim,Nb
    dim=0
    Nb=Ns-mz
    do i=0,Nb/2 
       dim=dim + 2**(Nb-2*i)*binomial(ns,Nb-2*i)*binomial(ns-Nb+2*i,i)
    enddo
  end function get_superc_sector_dimension
  !NONSU2
  function get_nonsu2_sector_dimension(n) result(dim)
    integer :: n
    integer :: dim
    dim=binomial(2*Ns,n)
  end function get_nonsu2_sector_dimension
  !NONSU2 - Jz conserving
  function get_nonsu2_sector_dimension_Jz(n,twoJz) result(dim)
    integer :: n
    integer :: twoJz
    integer :: dim
    integer :: ivec(Ns),jvec(Ns)
    integer :: iup,idw,ibath,iorb
    integer :: nt,twoLz,twoSz
    !
    dim=0
    do idw=0,2**Ns-1
       jvec = bdecomp(idw,Ns)
       do iup=0,2**Ns-1
          ivec = bdecomp(iup,Ns)
          nt   = sum(ivec) + sum(jvec)
          twoLz=0;twoSz=0
          do ibath=0,Nbath
             do iorb=1,Norb
                twoLz = twoLz + 2 * Lzdiag(iorb) * ivec(iorb+Norb*ibath)  &
                     + 2 * Lzdiag(iorb) * jvec(iorb+Norb*ibath)
             enddo
          enddo
          twoSz = (sum(ivec) - sum(jvec))
          !
          if(nt == n .and. twoJz==(twoSz+twoLz) )then
             dim=dim+1
          endif
       enddo
    enddo
  end function get_nonsu2_sector_dimension_Jz






  !+------------------------------------------------------------------+
  !PURPOSE  : calculate the binomial factor n1 over n2
  !+------------------------------------------------------------------+
  elemental function binomial(n1,n2) result(nchoos)
    integer,intent(in) :: n1,n2
    real(8)            :: xh
    integer            :: i
    integer nchoos
    xh = 1.d0
    if(n2<0) then
       nchoos = 0
       return
    endif
    if(n2==0) then
       nchoos = 1
       return
    endif
    do i = 1,n2
       xh = xh*dble(n1+1-i)/dble(i)
    enddo
    nchoos = int(xh + 0.5d0)
  end function binomial



end MODULE ED_SETUP












