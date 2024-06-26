MODULE ED_OBSERVABLES_NONSU2
  USE SF_CONSTANTS, only:zero,pi,xi
  USE SF_IOTOOLS, only:free_unit,reg,txtfy
  USE SF_ARRAYS, only: arange
  USE SF_LINALG
  USE ED_INPUT_VARS
  USE ED_VARS_GLOBAL
  USE ED_AUX_FUNX
  USE ED_EIGENSPACE
  USE ED_SETUP
  USE ED_SECTOR
  USE ED_BATH
  USE ED_HAMILTONIAN_NONSU2
  !
  implicit none
  private
  !
  public :: observables_nonsu2
  public :: local_energy_nonsu2


  logical,save                          :: iolegend=.true.
  real(8),dimension(:),allocatable      :: dens,dens_up,dens_dw
  real(8),dimension(:),allocatable      :: docc
  real(8),dimension(:),allocatable      :: magZ,magX,magY
  real(8),dimension(:),allocatable      :: phisc
  real(8),dimension(:,:),allocatable    :: sz2,n2
  real(8),dimension(:,:),allocatable    :: exct_s0
  real(8),dimension(:,:),allocatable    :: exct_tz
  real(8),dimension(:,:),allocatable    :: exct_tx
  real(8),dimension(:,:),allocatable    :: exct_ty
  real(8),dimensioN(:,:),allocatable    :: zimp,simp
  real(8)                               :: s2tot
  real(8)                               :: Egs
  real(8)                               :: Ei
  !
  integer                               :: iorb,jorb,istate
  integer                               :: ispin,jspin
  integer                               :: isite,jsite
  integer                               :: ibath
  integer                               :: r,m,k,k1,k2,k3,k4
  integer                               :: iup,idw
  integer                               :: jup,jdw
  integer                               :: mup,mdw
  integer                               :: iph,i_el,isz
  real(8)                               :: sgn,sgn1,sgn2,sg1,sg2,sg3,sg4
  real(8)                               :: gs_weight
  !
  real(8)                               :: peso
  real(8)                               :: norm
  !
  integer                               :: i,j,ii
  integer                               :: isector,jsector
  !
  complex(8),dimension(:),allocatable   :: vvinit
  complex(8),dimension(:),allocatable   :: state_cvec
  logical                               :: Jcondition
  !
  type(sector)                          :: sectorI,sectorJ



contains 



  !+-------------------------------------------------------------------+
  !PURPOSE  : Evaluate and print out many interesting physical qties
  !+-------------------------------------------------------------------+
  subroutine observables_nonsu2()
    integer,dimension(2*Ns)      :: ib
    integer,dimension(2,Ns)      :: Nud
    integer,dimension(Ns)        :: IbUp,IbDw
    real(8),dimension(Norb)      :: nup,ndw,Sz,nt
    real(8),dimension(Norb,Norb) :: theta_upup,theta_dwdw
    real(8),dimension(Norb,Norb) :: theta_updw,theta_dwup
    real(8),dimension(Norb,Norb) :: omega_updw,omega_dwup
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG observables_nonsu2"
#endif
    !
    !LOCAL OBSERVABLES:
    ! density, 
    ! double occupancy, 
    ! magnetization, 
    ! orbital//spin correlations  
    ! superconducting order parameter, etc..
    allocate(dens(Norb),dens_up(Norb),dens_dw(Norb))
    allocate(docc(Norb))
    allocate(magZ(Norb),sz2(Norb,Norb),n2(Norb,Norb))
    allocate(magX(Norb),magY(Norb))
    allocate(exct_S0(Norb,Norb),exct_Tz(Norb,Norb))
    allocate(exct_Tx(Norb,Norb),exct_Ty(Norb,Norb))
    allocate(simp(Norb,Nspin),zimp(Norb,Nspin))
    !
    Egs     = state_list%emin
    dens    = 0.d0
    dens_up = 0.d0
    dens_dw = 0.d0
    docc    = 0.d0
    magz    = 0.d0
    magx    = 0.d0
    magy    = 0.d0
    sz2     = 0.d0
    n2      = 0.d0
    s2tot   = 0.d0
    exct_s0 = 0d0
    exct_tz = 0d0
    exct_tx = 0d0
    exct_ty = 0d0
    theta_upup = 0d0
    theta_dwdw = 0d0
    theta_updw = 0d0
    theta_dwup = 0d0
    omega_updw = 0d0
    omega_dwup = 0d0
    !
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_nonsu2: get local observables"
#endif    
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_nonsu2: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_cvector(MpiComm,state_list,istate,state_cvec) 
       else
          call es_return_cvector(state_list,istate,state_cvec) 
       endif
#else
       call es_return_cvector(state_list,istate,state_cvec) 
#endif
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(Mpimaster)then
          call build_sector(isector,sectorI)
          do i = 1,sectorI%Dim
             gs_weight=peso*abs(state_cvec(i))**2
             !
             m  = sectorI%H(1)%map(i)
             ib = bdecomp(m,2*Ns)
             do iorb=1,Norb
                nup(iorb)= dble(ib(iorb))
                ndw(iorb)= dble(ib(iorb+Ns))
             enddo
             sz = (nup-ndw)/2d0
             nt =  nup+ndw
             !
             !Evaluate averages of observables:
             do iorb=1,Norb
                dens(iorb)     = dens(iorb)      +  nt(iorb)*gs_weight
                dens_up(iorb)  = dens_up(iorb)   +  nup(iorb)*gs_weight
                dens_dw(iorb)  = dens_dw(iorb)   +  ndw(iorb)*gs_weight
                docc(iorb)     = docc(iorb)      +  nup(iorb)*ndw(iorb)*gs_weight
                magz(iorb)     = magz(iorb)      +  (nup(iorb)-ndw(iorb))*gs_weight
                sz2(iorb,iorb) = sz2(iorb,iorb)  +  (sz(iorb)*sz(iorb))*gs_weight
                n2(iorb,iorb)  = n2(iorb,iorb)   +  (nt(iorb)*nt(iorb))*gs_weight
                do jorb=iorb+1,Norb
                   sz2(iorb,jorb) = sz2(iorb,jorb)  +  (sz(iorb)*sz(jorb))*gs_weight
                   sz2(jorb,iorb) = sz2(jorb,iorb)  +  (sz(jorb)*sz(iorb))*gs_weight
                   n2(iorb,jorb)  = n2(iorb,jorb)   +  (nt(iorb)*nt(jorb))*gs_weight
                   n2(jorb,iorb)  = n2(jorb,iorb)   +  (nt(jorb)*nt(iorb))*gs_weight
                enddo
             enddo
             s2tot = s2tot  + (sum(sz))**2*gs_weight
          enddo
       endif
       if(allocated(state_cvec))deallocate(state_cvec)
       !
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif




    !EVALUATE <SX> AND <SY>
    do iorb=1,Norb
       !
#ifdef _DEBUG
       if(ed_verbose>2)write(Logfile,"(A)")&
            "DEBUG observables_nonsu2: eval in-plane magnetization <Sx>, <Sy>, a:"//str(iorb)
#endif
       do istate=1,state_list%size
          isector = es_return_sector(state_list,istate)
          Ei      = es_return_energy(state_list,istate)
          !
#ifdef _DEBUG
          if(ed_verbose>3)write(Logfile,"(A)")&
               "DEBUG observables_nonsu2: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
          if(MpiStatus)then
             call es_return_cvector(MpiComm,state_list,istate,state_cvec) 
          else
             call es_return_cvector(state_list,istate,state_cvec) 
          endif
#else
          call es_return_cvector(state_list,istate,state_cvec) 
#endif
          !
          peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
          peso = peso/zeta_function
          !
          !GET <(CDG_UP + CDG_DW)(C_UP + C_DW)> = 
          !<CDG_UP*C_UP> + <CDG_DW*C_DW> + <CDG_UP*C_DW + CDG_DW*C_UP> = 
          !<N_UP> + <N_DW> + <Sx> 
          !since <Sx> = <CDG_UP*C_DW + CDG_DW*C_UP> 
          jsector = getCsector(1,1,isector)
          if(jsector/=0)then
             if(Mpimaster)then
                call build_sector(isector,sectorI)
                call build_sector(jsector,sectorJ)
                allocate(vvinit(sectorJ%Dim));vvinit=zero
                do i=1,sectorI%Dim
                   call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !c_a,up
                   if(sgn==0d0.OR.j==0)cycle
                   vvinit(j) = sgn*state_cvec(i)
                enddo
                do i=1,sectorI%Dim
                   call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !c_a,dw
                   if(sgn==0d0.OR.j==0)cycle
                   vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                enddo
                call delete_sector(sectorI)
                call delete_sector(sectorJ)
                magx(iorb) = magx(iorb) + dot_product(vvinit,vvinit)*peso
                if(allocated(vvinit))deallocate(vvinit)
             endif
          endif
          !
          !GET <(-i*CDG_UP + CDG_DW)(i*C_UP + C_DW)> = 
          !<CDG_UP*C_UP> + <CDG_DW*C_DW> - i<CDG_UP*C_DW - CDG_DW*C_UP> = 
          !<N_UP> + <N_DW> + <Sy>
          !since <Sy> = -i<CDG_UP*C_DW - CDG_DW*C_UP>         
          jsector = getCsector(1,1,isector)
          if(jsector/=0)then
             if(Mpimaster)then
                call build_sector(isector,sectorI)
                call build_sector(jsector,sectorJ)
                allocate(vvinit(sectorJ%Dim));vvinit=zero
                do i=1,sectorI%Dim
                   call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !i*c_a,up
                   if(sgn==0d0.OR.j==0)cycle
                   vvinit(j) = xi*sgn*state_cvec(i)
                enddo
                do i=1,sectorI%Dim
                   call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !c_a,dw
                   if(sgn==0d0.OR.j==0)cycle
                   vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                enddo
                call delete_sector(sectorI)
                call delete_sector(sectorJ)
                magy(iorb) = magy(iorb) + dot_product(vvinit,vvinit)*peso
                if(allocated(vvinit))deallocate(vvinit)
             endif
          endif
          if(allocated(state_cvec))deallocate(state_cvec)
       enddo
       !So we have:
       !<Sx> = <(CDG_UP + CDG_DW)(C_UP + C_DW)> - <N_UP> - <N_DW>
       magx(iorb) = magx(iorb) - dens_up(iorb) - dens_dw(iorb)
       !<Sy> = <(-i*CDG_UP + CDG_DW)(i*C_UP + C_DW)> - <N_UP> - <N_DW 
       magy(iorb) = magy(iorb) - dens_up(iorb) - dens_dw(iorb)
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif



    !EVALUATE EXCITON OP <S_ab> AND <T^x,y,z_ab>
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_nonsu2: eval excitoninc OP <S_av>, <T^{x,y,z}_ab>"
#endif
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_nonsu2: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_cvector(MpiComm,state_list,istate,state_cvec) 
       else
          call es_return_cvector(state_list,istate,state_cvec) 
       endif
#else
       call es_return_cvector(state_list,istate,state_cvec) 
#endif
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(Mpimaster)call build_sector(isector,sectorI)
       !
       !<S_ab>  :=   <C^+_{a,up}C_{b,up} + C^+_{a,dw}C_{b,dw}>
       !<T^z_ab>:=   <C^+_{a,up}C_{b,up} - C^+_{a,dw}C_{b,dw}>
       ! O_uu  = a_up + b_up
       ! O_dd  = a_dw + b_dw
       !|v_up> = O_uu|v>
       !|v_dw> = O_dd|v> 
       ! Theta_uu = <v_up|v_up>
       ! Theta_dd = <v_dw|v_dw>
       do iorb=1,Norb
          do jorb=iorb+1,Norb
             !
             !|v_up> = (C_aup + C_bup)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,1,sectorI,sectorJ) !c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !+c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !
                   theta_upup(iorb,jorb) = theta_upup(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
             !|v_dw> = (C_adw + C_bdw)|>
             jsector = getCsector(1,2,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,2,sectorI,sectorJ) !c_b,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !+c_a,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !
                   theta_dwdw(iorb,jorb) = theta_dwdw(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
          enddo
       enddo
       !
       !<T^x_ab>:=   <C^+_{a,up}C_{b,dw} + C^+_{a,dw}C_{b,up}>
       ! O_ud  = a_up + b_dw
       ! O_du  = a_dw + b_up
       !|v_ud> = O_ud|v>
       !|v_du> = O_du|v> 
       ! Theta_ud = <v_ud|v_ud>
       ! Theta_du = <v_du|v_du>       
       do iorb=1,Norb
          do jorb=iorb+1,Norb
             !
             !|v_ud> = (C_aup + C_bdw)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,2,sectorI,sectorJ) !c_b,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !+c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !Theta_ud = <v_ud|v_ud>
                   theta_updw(iorb,jorb) = theta_updw(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
             !|v_du> = (C_adw + C_bup)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,1,sectorI,sectorJ) !c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !+c_a,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !Theta_du = <v_du|v_du>
                   theta_dwup(iorb,jorb) = theta_dwup(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
          enddo
       enddo
       !
       !<T^y_ab>:= -i<C^+_{a,up}C_{b,dw} - C^+_{a,dw}C_{b,up}>
       ! K_ud  = a_up - i*b_dw
       ! K_du  = a_dw - i*b_up
       !|w_ud> = K_ud|v>
       !|w_du> = K_du|v> 
       ! Omega_ud = <v_ud|v_ud>
       ! Omega_du = <v_du|v_du>    
       do iorb=1,Norb
          do jorb=iorb+1,Norb
             !|w_ud> = (C_aup - xi*C_bdw)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,2,sectorI,sectorJ) !-i*c_b,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = -xi*sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !+c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !Omega_ud = <w_ud|w_ud>
                   omega_updw(iorb,jorb) = omega_updw(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
             !|w_du> = (C_adw - i*C_bup)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,1,sectorI,sectorJ) !-i*c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = -xi*sgn*state_cvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !+c_a,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !Omega_du = <w_du|w_du>
                   omega_dwup(iorb,jorb) = omega_dwup(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
          enddo
       enddo
       !
       if(allocated(state_cvec))deallocate(state_cvec)
       !
    enddo
    ! <S_ab>  = Theta_uu + Theta_dd - n_a - n_b
    ! <T^z_ab>= Theta_uu - Theta_dd - m_a - m_b
    ! <T^x_ab>= Theta_ud + Theta_du - n_a - n_b
    ! <T^y_ab>= Omega_ud - Omega_du - m_a - m_b
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          exct_s0(iorb,jorb) = theta_upup(iorb,jorb) + theta_dwdw(iorb,jorb) - dens(iorb) - dens(jorb)
          exct_tz(iorb,jorb) = theta_upup(iorb,jorb) - theta_dwdw(iorb,jorb) - magZ(iorb) - magZ(jorb)
          exct_tx(iorb,jorb) = theta_updw(iorb,jorb) + theta_dwup(iorb,jorb) - dens(iorb) - dens(jorb)
          exct_ty(iorb,jorb) = omega_updw(iorb,jorb) - omega_dwup(iorb,jorb) - magZ(iorb) + magZ(jorb)
       enddo
    enddo



    !IMPURITY DENSITY MATRIX
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_nonsu2: eval impurity density matrix <C^+_a C_b>"
#endif
    if(allocated(imp_density_matrix))deallocate(imp_density_matrix)
    allocate(imp_density_matrix(Nspin,Nspin,Norb,Norb))
    imp_density_matrix=zero
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_nonsu2: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_cvector(MpiComm,state_list,istate,state_cvec) 
       else
          call es_return_cvector(state_list,istate,state_cvec) 
       endif
#else
       call es_return_cvector(state_list,istate,state_cvec) 
#endif
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(Mpimaster)then
          call build_sector(isector,sectorI)
          !Diagonal densities
          do ispin=1,Nspin
             do iorb=1,Norb
                isite=iorb + (ispin-1)*Norb
                do m=1,sectorI%Dim
                   i  = sectorI%H(1)%map(m)
                   ib = bdecomp(i,2*Ns)
                   imp_density_matrix(ispin,ispin,iorb,iorb) = &
                        imp_density_matrix(ispin,ispin,iorb,iorb) + &
                        peso*ib(isite)*conjg(state_cvec(m))*state_cvec(m)
                enddo
             enddo
          enddo
          !off-diagonal
          do ispin=1,Nspin
             do jspin=1,Nspin
                do iorb=1,Norb
                   do jorb=1,Norb
                      if((bath_type=="normal").and.(iorb/=jorb))cycle
                      ! if(bath_type=="replica".and.Jz_basis)then
                      !    if((.not.dmft_bath%mask(ispin,jspin,iorb,jorb,1)).AND.&
                      !         (.not.dmft_bath%mask(ispin,jspin,iorb,jorb,2)))cycle
                      ! endif
                      isite=iorb + (ispin-1)*Norb
                      jsite=jorb + (jspin-1)*Norb
                      do m=1,sectorI%Dim
                         i  = sectorI%H(1)%map(m)
                         ib = bdecomp(i,2*Ns)
                         if((ib(jsite)==1).and.(ib(isite)==0))then
                            call c(jsite,i,r,sgn1)
                            call cdg(isite,r,k,sgn2)
                            j=binary_search(sectorI%H(1)%map,k)
                            imp_density_matrix(ispin,jspin,iorb,jorb) = &
                                 imp_density_matrix(ispin,jspin,iorb,jorb) + &
                                 peso*sgn1*state_cvec(m)*sgn2*conjg(state_cvec(j))
                         endif
                      enddo
                   enddo
                enddo
             enddo
          enddo
          call delete_sector(sectorI)
       endif
       if(allocated(state_cvec))deallocate(state_cvec)
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif

    !
    !
    if(MPIMASTER)then
       call get_szr
       if(iolegend)call write_legend
       call write_observables()
    endif
    write(LOGfile,"(A,10f18.12,f18.12)")"dens"//reg(ed_file_suffix)//"=",(dens(iorb),iorb=1,Norb),sum(dens)
    write(LOGfile,"(A,10f18.12)")    "docc"//reg(ed_file_suffix)//"=",(docc(iorb),iorb=1,Norb)
    write(LOGfile,"(A,10f18.12)")    "magX"//reg(ed_file_suffix)//"=",(magX(iorb),iorb=1,Norb)
    write(LOGfile,"(A,10f18.12)")    "magY"//reg(ed_file_suffix)//"=",(magY(iorb),iorb=1,Norb)
    write(LOGfile,"(A,10f18.12)")    "magZ"//reg(ed_file_suffix)//"=",(magz(iorb),iorb=1,Norb)
    !
    write(LOGfile,"(A)",advance="no")"exS0"//reg(ed_file_suffix)//"="
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(LOGfile,"(20(F18.12,1X))")exct_s0(iorb,jorb)
       enddo
    enddo
    write(LOGfile,"(A)",advance="no")"exTX"//reg(ed_file_suffix)//"="
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(LOGfile,"(20(F18.12,1X))")exct_tx(iorb,jorb)
       enddo
    enddo
    write(LOGfile,"(A)",advance="no")"exTY"//reg(ed_file_suffix)//"="
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(LOGfile,"(20(F18.12,1X))")exct_ty(iorb,jorb)
       enddo
    enddo
    write(LOGfile,"(A)",advance="no")"exTZ"//reg(ed_file_suffix)//"="
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(LOGfile,"(20(F18.12,1X))")exct_tz(iorb,jorb)
       enddo
    enddo
    !
    do iorb=1,Norb
       ed_dens_up(iorb)=dens_up(iorb)
       ed_dens_dw(iorb)=dens_dw(iorb)
       ed_dens(iorb)   =dens(iorb)
       ed_docc(iorb)   =docc(iorb)
       ed_mag(1,iorb)  =magX(iorb)
       ed_mag(2,iorb)  =magY(iorb)
       ed_mag(3,iorb)  =magZ(iorb)
    enddo
#ifdef _MPI
    if(MpiStatus)then
       call Bcast_MPI(MpiComm,ed_dens_up)
       call Bcast_MPI(MpiComm,ed_dens_dw)
       call Bcast_MPI(MpiComm,ed_dens)
       call Bcast_MPI(MpiComm,ed_docc)
       call Bcast_MPI(MpiComm,ed_mag)
       if(allocated(imp_density_matrix))call Bcast_MPI(MpiComm,imp_density_matrix)
    endif
#endif
    !
    deallocate(dens,docc,dens_up,dens_dw,magz,sz2,n2)
    deallocate(magX,magY)
    deallocate(exct_S0,exct_Tz)
    deallocate(exct_Tx,exct_Ty)
    deallocate(simp,zimp)
  end subroutine observables_nonsu2







  !+-------------------------------------------------------------------+
  !PURPOSE  : Get internal energy from the Impurity problem.
  !+-------------------------------------------------------------------+
  subroutine local_energy_nonsu2()
    integer,dimension(2*Ns) :: ib
    integer,dimension(2,Ns) :: Nud
    integer,dimension(Ns)   :: IbUp,IbDw
    real(8),dimension(Norb)         :: nup,ndw
    real(8),dimension(Nspin,Norb)   :: eloc
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG local_energy_nonsu2"
#endif
    !
    Egs     = state_list%emin
    ed_Ehartree= 0.d0
    ed_Eknot   = 0.d0
    ed_Epot    = 0.d0
    ed_Dust    = 0.d0
    ed_Dund    = 0.d0
    ed_Dse     = 0.d0
    ed_Dph     = 0.d0
    !
    !Get diagonal part of Hloc
    do ispin=1,Nspin
       do iorb=1,Norb
          eloc(ispin,iorb)=impHloc(ispin,ispin,iorb,iorb)
       enddo
    enddo
    !
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG local_energy_nonsu2: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_cvector(MpiComm,state_list,istate,state_cvec) 
       else
          call es_return_cvector(state_list,istate,state_cvec) 
       endif
#else
       call es_return_cvector(state_list,istate,state_cvec) 
#endif
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(Mpimaster)then
          !
          call build_sector(isector,sectorI)
          do i=1,sectorI%Dim
             gs_weight=peso*abs(state_cvec(i))**2
             m  = sectorI%H(1)%map(i)
             ib = bdecomp(m,2*Ns)
             do iorb=1,Norb
                nup(iorb)= dble(ib(iorb))
                ndw(iorb)= dble(ib(iorb+Ns))
             enddo
             !
             !start evaluating the Tr(H_loc) to estimate potential energy
             !LOCAL ENERGY
             ed_Eknot = ed_Eknot + dot_product(eloc(1,:),nup)*gs_weight + dot_product(eloc(Nspin,:),ndw)*gs_weight
             !==> HYBRIDIZATION TERMS I: same or different orbitals, same spins.
             do iorb=1,Norb
                do jorb=1,Norb
                   !SPIN UP
                   if((ib(iorb)==0).AND.(ib(jorb)==1))then
                      call c(jorb,m,k1,sg1)
                      call cdg(iorb,k1,k2,sg2)
                      j=binary_search(sectorI%H(1)%map,k2)
                      if(Jz_basis.and.j==0)cycle
                      ed_Eknot = ed_Eknot + impHloc(1,1,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
                   endif
                   !SPIN DW
                   if((ib(iorb+Ns)==0).AND.(ib(jorb+Ns)==1))then
                      call c(jorb+Ns,m,k1,sg1)
                      call cdg(iorb+Ns,k1,k2,sg2)
                      j=binary_search(sectorI%H(1)%map,k2)
                      if(Jz_basis.and.j==0)cycle
                      ed_Eknot = ed_Eknot + impHloc(Nspin,Nspin,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
                   endif
                enddo
             enddo
             !==> HYBRIDIZATION TERMS II: same or different orbitals, opposite spins.
             do iorb=1,Norb
                do jorb=1,Norb
                   !UP-DW
                   if((impHloc(1,Nspin,iorb,jorb)/=zero).AND.(ib(iorb)==0).AND.(ib(jorb+Ns)==1))then
                      call c(jorb+Ns,m,k1,sg1)
                      call cdg(iorb,k1,k2,sg2)
                      j=binary_search(sectorI%H(1)%map,k2)
                      if(Jz_basis.and.j==0)cycle
                      ed_Eknot = ed_Eknot + impHloc(1,Nspin,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
                   endif
                   !DW-UP
                   if((impHloc(Nspin,1,iorb,jorb)/=zero).AND.(ib(iorb+Ns)==0).AND.(ib(jorb)==1))then
                      call c(jorb,m,k1,sg1)
                      call cdg(iorb+Ns,k1,k2,sg2)
                      j=binary_search(sectorI%H(1)%map,k2)
                      if(Jz_basis.and.j==0)cycle
                      ed_Eknot = ed_Eknot + impHloc(Nspin,1,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
                   endif
                enddo
             enddo
             !
             !DENSITY-DENSITY INTERACTION: SAME ORBITAL, OPPOSITE SPINS
             !Euloc=\sum=i U_i*(n_u*n_d)_i
             !ed_Epot = ed_Epot + dot_product(uloc,nup*ndw)*gs_weight
             do iorb=1,Norb
                ed_Epot = ed_Epot + Uloc(iorb)*nup(iorb)*ndw(iorb)*gs_weight
             enddo
             !
             !DENSITY-DENSITY INTERACTION: DIFFERENT ORBITALS, OPPOSITE SPINS
             !Eust=\sum_ij Ust*(n_up_i*n_dn_j + n_up_j*n_dn_i)
             !    "="\sum_ij (Uloc - 2*Jh)*(n_up_i*n_dn_j + n_up_j*n_dn_i)
             if(Norb>1)then
                do iorb=1,Norb
                   do jorb=iorb+1,Norb
                      ed_Epot = ed_Epot + Ust*(nup(iorb)*ndw(jorb) + nup(jorb)*ndw(iorb))*gs_weight
                      ed_Dust = ed_Dust + (nup(iorb)*ndw(jorb) + nup(jorb)*ndw(iorb))*gs_weight
                   enddo
                enddo
             endif
             !
             !DENSITY-DENSITY INTERACTION: DIFFERENT ORBITALS, PARALLEL SPINS
             !Eund = \sum_ij Und*(n_up_i*n_up_j + n_dn_i*n_dn_j)
             !    "="\sum_ij (Ust-Jh)*(n_up_i*n_up_j + n_dn_i*n_dn_j)
             !    "="\sum_ij (Uloc-3*Jh)*(n_up_i*n_up_j + n_dn_i*n_dn_j)
             if(Norb>1)then
                do iorb=1,Norb
                   do jorb=iorb+1,Norb
                      ed_Epot = ed_Epot + (Ust-Jh)*(nup(iorb)*nup(jorb) + ndw(iorb)*ndw(jorb))*gs_weight
                      ed_Dund = ed_Dund + (nup(iorb)*nup(jorb) + ndw(iorb)*ndw(jorb))*gs_weight
                   enddo
                enddo
             endif
             !
             !SPIN-EXCHANGE (S-E) TERMS
             !S-E: Jh *( c^+_iorb_up c^+_jorb_dw c_iorb_dw c_jorb_up )  (i.ne.j) 
             if(Norb>1.AND.Jhflag)then
                do iorb=1,Norb
                   do jorb=1,Norb
                      Jcondition=((iorb/=jorb).AND.&
                           (ib(jorb)==1)      .AND.&
                           (ib(iorb+Ns)==1)   .AND.&
                           (ib(jorb+Ns)==0)   .AND.&
                           (ib(iorb)==0))
                      if(Jcondition)then
                         call c(jorb,m,k1,sg1)
                         call c(iorb+Ns,k1,k2,sg2)
                         call cdg(jorb+Ns,k2,k3,sg3)
                         call cdg(iorb,k3,k4,sg4)
                         j=binary_search(sectorI%H(1)%map,k4)
                         if(Jz_basis.and.j==0)cycle
                         ed_Epot = ed_Epot + Jx*sg1*sg2*sg3*sg4*state_cvec(i)*conjg(state_cvec(j))*peso
                         ed_Dse  = ed_Dse  + sg1*sg2*sg3*sg4*state_cvec(i)*conjg(state_cvec(j))*peso
                      endif
                   enddo
                enddo
             endif
             !
             !
             !PAIR-HOPPING (P-H) TERMS
             !P-H: J c^+_iorb_up c^+_iorb_dw   c_jorb_dw   c_jorb_up  (i.ne.j) 
             !P-H: J c^+_{iorb}  c^+_{iorb+Ns} c_{jorb+Ns} c_{jorb}
             if(Norb>1.AND.Jhflag)then
                do iorb=1,Norb
                   do jorb=1,Norb
                      Jcondition=((iorb/=jorb).AND.&
                           (ib(jorb)==1)      .AND.&
                           (ib(jorb+Ns)==1)   .AND.&
                           (ib(iorb+Ns)==0)   .AND.&
                           (ib(iorb)==0))
                      if(Jcondition)then
                         call c(jorb,m,k1,sg1)
                         call c(jorb+Ns,k1,k2,sg2)
                         call cdg(iorb+Ns,k2,k3,sg3)
                         call cdg(iorb,k3,k4,sg4)
                         j=binary_search(sectorI%H(1)%map,k4)
                         if(Jz_basis.and.j==0)cycle
                         ed_Epot = ed_Epot + Jp*sg1*sg2*sg3*sg4*state_cvec(i)*conjg(state_cvec(j))*peso
                         ed_Dph  = ed_Dph  + sg1*sg2*sg3*sg4*state_cvec(i)*conjg(state_cvec(j))*peso
                      endif
                   enddo
                enddo
             endif
             !
             !
             !HARTREE-TERMS CONTRIBUTION:
             if(hfmode)then
                !ed_Ehartree=ed_Ehartree - 0.5d0*dot_product(uloc,nup+ndw)*gs_weight + 0.25d0*sum(uloc)*gs_weight
                do iorb=1,Norb
                   ed_Ehartree=ed_Ehartree - 0.5d0*uloc(iorb)*(nup(iorb)+ndw(iorb))*gs_weight + 0.25d0*uloc(iorb)*gs_weight
                enddo
                if(Norb>1)then
                   do iorb=1,Norb
                      do jorb=iorb+1,Norb
                         ed_Ehartree=ed_Ehartree - 0.5d0*Ust*(nup(iorb)+ndw(iorb)+nup(jorb)+ndw(jorb))*gs_weight + 0.5d0*Ust*gs_weight
                         ed_Ehartree=ed_Ehartree - 0.5d0*(Ust-Jh)*(nup(iorb)+ndw(iorb)+nup(jorb)+ndw(jorb))*gs_weight + 0.5d0*(Ust-Jh)*gs_weight
                      enddo
                   enddo
                endif
             endif
          enddo
          call delete_sector(sectorI)
       endif
       !
       if(allocated(state_cvec))deallocate(state_cvec)
       !
    enddo
    !
#ifdef _DEBUG
    write(Logfile,"(A)")""
#endif
#ifdef _MPI
    if(MpiStatus)then
       call Bcast_MPI(MpiComm,ed_Epot)
       call Bcast_MPI(MpiComm,ed_Eknot)
       call Bcast_MPI(MpiComm,ed_Ehartree)
       call Bcast_MPI(MpiComm,ed_Dust)
       call Bcast_MPI(MpiComm,ed_Dund)
       call Bcast_MPI(MpiComm,ed_Dse)
       call Bcast_MPI(MpiComm,ed_Dph)
    endif
#endif
    !
    ed_Epot = ed_Epot + ed_Ehartree
    !
    if(ed_verbose>=3)then
       write(LOGfile,"(A,10f18.12)")"<Hint>  =",ed_Epot
       write(LOGfile,"(A,10f18.12)")"<V>     =",ed_Epot-ed_Ehartree
       write(LOGfile,"(A,10f18.12)")"<E0>    =",ed_Eknot
       write(LOGfile,"(A,10f18.12)")"<Ehf>   =",ed_Ehartree    
       write(LOGfile,"(A,10f18.12)")"Dust    =",ed_Dust
       write(LOGfile,"(A,10f18.12)")"Dund    =",ed_Dund
       write(LOGfile,"(A,10f18.12)")"Dse     =",ed_Dse
       write(LOGfile,"(A,10f18.12)")"Dph     =",ed_Dph
    endif
    if(MPIMASTER)then
       call write_energy_info()
       call write_energy()
    endif
    !
    !
  end subroutine local_energy_nonsu2



  !####################################################################
  !                    COMPUTATIONAL ROUTINES
  !####################################################################
  !+-------------------------------------------------------------------+
  !PURPOSE  : get scattering rate and renormalization constant Z
  !+-------------------------------------------------------------------+
  subroutine get_szr()
    integer                  :: ispin,iorb
    real(8)                  :: wm1,wm2
    wm1 = pi/beta ; wm2=3d0*pi/beta
    do ispin=1,Nspin
       do iorb=1,Norb
          simp(iorb,ispin) = dimag(impSmats(ispin,ispin,iorb,iorb,1)) - &
               wm1*(dimag(impSmats(ispin,ispin,iorb,iorb,2))-dimag(impSmats(ispin,ispin,iorb,iorb,1)))/(wm2-wm1)
          zimp(iorb,ispin)   = 1.d0/( 1.d0 + abs( dimag(impSmats(ispin,ispin,iorb,iorb,1))/wm1 ))
       enddo
    enddo
  end subroutine get_szr



  !+-------------------------------------------------------------------+
  !PURPOSE  : write legend, i.e. info about columns 
  !+-------------------------------------------------------------------+
  subroutine write_legend()
    integer :: unit,iorb,jorb,ispin
    !
    unit = free_unit()
    open(unit,file="observables_info.ed")
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# dens_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"dens_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# docc_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"docc_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# dens_up_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"dens_up_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# dens_dw_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"dens_dw_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# magX_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"magX_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# magY_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"magY_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# magZ_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"magZ_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# Sz2_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",((reg(txtfy(iorb+(jorb-1)*Norb))//"Sz2_"//reg(txtfy(iorb)),iorb=1,Norb),jorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# n2_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",((reg(txtfy(iorb+(jorb-1)*Norb))//"n2_"//reg(txtfy(iorb)),iorb=1,Norb),jorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# Z_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",((reg(txtfy(iorb+(ispin-1)*Norb))//"z_"//reg(txtfy(iorb))//"s"//reg(txtfy(ispin)),iorb=1,Norb),ispin=1,Nspin)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# sig_last.ed"
    write(unit,"(A1,90(A10,6X))") "#",((reg(txtfy(iorb+(ispin-1)*Norb))//"sig_"//reg(txtfy(iorb))//"s"//reg(txtfy(ispin)),iorb=1,Norb),ispin=1,Nspin)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# phisc_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"phisc_"//reg(txtfy(iorb)),iorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# imp_last.ed"
    write(unit,"(A1,90(A10,6X))") "#", "1s2tot", "2egs"
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# exciton_last.ed"
    write(unit,"(A1,90(A10,6X))") "#","1S_0" , "2T_z", "3T_x", "4T_y"
    close(unit)
    !
    unit = free_unit()
    open(unit,file="parameters_info.ed")
    write(unit,"(A1,90(A14,1X))")"#","1xmu","2beta",&
         (reg(txtfy(2+iorb))//"U_"//reg(txtfy(iorb)),iorb=1,Norb),&
         reg(txtfy(2+Norb+1))//"U'",reg(txtfy(2+Norb+2))//"Jh"
    close(unit)
    !
    iolegend=.false.
  end subroutine write_legend

  subroutine write_energy_info()
    integer :: unit
    unit = free_unit()
    open(unit,file="energy_info.ed")
    write(unit,"(A1,90(A14,1X))")"#",&
         reg(txtfy(1))//"<Hi>",&
         reg(txtfy(2))//"<V>=<Hi-Ehf>",&
         reg(txtfy(3))//"<Eloc>",&
         reg(txtfy(4))//"<Ehf>",&
         reg(txtfy(5))//"<Dst>",&
         reg(txtfy(6))//"<Dnd>",&
         reg(txtfy(7))//"<Dse>",&
         reg(txtfy(8))//"<Dph>"
    close(unit)
  end subroutine write_energy_info


  !+-------------------------------------------------------------------+
  !PURPOSE  : write observables to file
  !+-------------------------------------------------------------------+
  subroutine write_observables()
    integer :: unit
    integer :: iorb,jorb,ispin
    !
    !ALL OBSERVABLES
    unit = free_unit()
    open(unit,file="dens_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (dens(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="docc_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (docc(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="dens_up_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (dens_up(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="dens_dw_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (dens_dw(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magX_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (magx(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magY_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (magy(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magZ_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") (magz(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Sz2_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") ((sz2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="n2_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") ((n2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Z_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") ((zimp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="sig_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") ((simp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    ! unit = free_unit()
    ! open(unit,file="phisc_all"//reg(ed_file_suffix)//".ed",position='append')
    ! write(unit,"(90(F15.9,1X))") (phisc(iorb),iorb=1,Norb)
    ! close(unit)
    !
    unit = free_unit()
    open(unit,file="imp_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") s2tot, egs
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exciton_all"//reg(ed_file_suffix)//".ed",position='append')
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")&
               exct_s0(iorb,jorb),exct_tz(iorb,jorb),exct_tx(iorb,jorb),exct_ty(iorb,jorb)
       enddo
    enddo
    close(unit)
    !
    !LAST OBSERVABLES
    unit = free_unit()
    open(unit,file="dens_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (dens(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="docc_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (docc(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="dens_up_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (dens_up(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="dens_dw_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (dens_dw(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magX_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (magx(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magY_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (magy(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="magZ_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (magz(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Sz2_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") ((sz2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="n2_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") ((n2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Z_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") ((zimp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="sig_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") ((simp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    !unit = free_unit()
    !open(unit,file="phisc_last"//reg(ed_file_suffix)//".ed")
    !write(unit,"(90(F15.9,1X))") (phisc(iorb),iorb=1,Norb)
    !close(unit)
    !
    unit = free_unit()
    open(unit,file="imp_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") s2tot, egs
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exciton_last"//reg(ed_file_suffix)//".ed")
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")&
               exct_s0(iorb,jorb),exct_tz(iorb,jorb),exct_tx(iorb,jorb),exct_ty(iorb,jorb)
       enddo
    enddo
    close(unit)
    !
    unit = free_unit()
    open(unit,file="parameters_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90F15.9)")xmu,beta,(uloc(iorb),iorb=1,Norb),Ust,Jh,Jx,Jp
    close(unit)

  end subroutine write_observables

  subroutine write_energy()
    integer :: unit
    unit = free_unit()
    open(unit,file="energy_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90F15.9)")ed_Epot,ed_Epot-ed_Ehartree,ed_Eknot,ed_Ehartree,ed_Dust,ed_Dund,ed_Dse,ed_Dph
    close(unit)
  end subroutine write_energy



end MODULE ED_OBSERVABLES_NONSU2
