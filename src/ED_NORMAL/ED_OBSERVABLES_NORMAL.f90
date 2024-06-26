MODULE ED_OBSERVABLES_NORMAL
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
  USE ED_HAMILTONIAN_NORMAL
  implicit none
  private
  !
  public :: observables_normal
  public :: local_energy_normal

  logical,save                       :: iolegend=.true.
  real(8),dimension(:),allocatable   :: dens,dens_up,dens_dw
  real(8),dimension(:),allocatable   :: docc
  real(8),dimension(:),allocatable   :: magz
  real(8),dimension(:,:),allocatable :: sz2,n2
  real(8),dimension(:,:),allocatable :: exct_s0, exct_s02
  real(8),dimension(:,:),allocatable :: exct_tz, exct_tz2
  real(8),dimension(:,:),allocatable :: zimp,simp
  real(8)                            :: dens_ph
  real(8)                            :: X_ph, X2_ph
  real(8)                            :: s2tot
  real(8)                            :: Egs
  real(8)                            :: Ei
  real(8),dimension(:),allocatable   :: Prob
  real(8),dimension(:),allocatable   :: prob_ph
  real(8),dimension(:),allocatable   :: pdf_ph
  real(8),dimension(:,:),allocatable :: pdf_part
  real(8)                            :: w_ph
  !
  integer                            :: iorb,jorb,iorb1,jorb1
  integer                            :: ispin,jspin
  integer                            :: isite,jsite
  integer                            :: ibath
  integer                            :: r,m,k,k1,k2,k3,k4
  integer                            :: iup,idw
  integer                            :: jup,jdw
  integer                            :: mup,mdw
  integer                            :: iph,i_el,isectorDim,j_el
  real(8)                            :: sgn,sgn1,sgn2,sg1,sg2,sg3,sg4
  real(8)                            :: gs_weight
  !
  real(8)                            :: peso
  real(8)                            :: norm
  !
  integer                            :: i,j,ii
  integer                            :: isector,jsector
  !
  real(8),dimension(:),allocatable   :: vvinit, vupinit, vdwinit
  real(8),dimension(:),allocatable   :: state_dvec
  logical                            :: Jcondition
  !
  type(sector)                       :: sectorI,sectorJ


contains 

  !+-------------------------------------------------------------------+
  !PURPOSE  : Lanc method
  !+-------------------------------------------------------------------+
  subroutine observables_normal()
    integer                         :: iprob,istate,Nud(2,Ns),iud(2),jud(2),val
    integer,dimension(2*Ns_Ud)      :: Indices,Jndices
    integer,dimension(Ns_Ud,Ns_Orb) :: Nups,Ndws  ![1,Ns]-[Norb,1+Nbath]
    integer,dimension(Ns)           :: IbUp,IbDw  ![Ns]
    real(8),dimension(Norb)         :: nup,ndw,Sz,nt
    real(8),dimension(Norb,Norb)    :: theta_upup,theta_dwdw
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG observables_normal"
#endif
    allocate(dens(Norb),dens_up(Norb),dens_dw(Norb))
    allocate(docc(Norb))
    allocate(magz(Norb),sz2(Norb,Norb),n2(Norb,Norb))
    allocate(simp(Norb,Nspin),zimp(Norb,Nspin))
    allocate(exct_S0(Norb,Norb),exct_Tz(Norb,Norb))
    allocate(exct_S02(Norb,Norb),exct_Tz2(Norb,Norb))
    allocate(Prob(3**Norb))
    allocate(prob_ph(DimPh))
    allocate(pdf_ph(Lpos))
    allocate(pdf_part(Lpos,3))
    !
    Egs     = state_list%emin
    dens    = 0.d0
    dens_up = 0.d0
    dens_dw = 0.d0
    docc    = 0.d0
    magz    = 0.d0
    sz2     = 0.d0
    n2      = 0.d0
    s2tot   = 0.d0
    exct_s0 = 0d0; exct_s02 = 0.d0
    exct_tz = 0d0; exct_tz2 = 0.d0
    theta_upup = 0d0
    theta_dwdw = 0d0
    Prob    = 0.d0
    prob_ph = 0.d0
    dens_ph = 0.d0
    X_ph = 0.d0
    X2_ph= 0.d0
    pdf_ph  = 0.d0
    pdf_part= 0.d0
    w_ph    = w0_ph
    !
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_normal: get local observables"
#endif
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_normal: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_dvector(MpiComm,state_list,istate,state_dvec) 
       else
          call es_return_dvector(state_list,istate,state_dvec) 
       endif
#else
       call es_return_dvector(state_list,istate,state_dvec) 
#endif
       !
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(MpiMaster)then
          call build_sector(isector,sectorI)
          do i = 1,sectorI%Dim
             gs_weight=peso*abs(state_dvec(i))**2
             call build_op_Ns(i,IbUp,IbDw,sectorI)
             nup = IbUp(1:Norb)
             ndw = IbDw(1:Norb)
             sz = (nup-ndw)/2d0
             nt =  nup+ndw
             !
             !Configuration probability
             iprob=1
             do iorb=1,Norb
                iprob=iprob+nint(nt(iorb))*3**(iorb-1)
             end do
             Prob(iprob) = Prob(iprob) + gs_weight
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
             !
             iph = (i-1)/(sectorI%DimEl) + 1
             i_el = mod(i-1,sectorI%DimEl) + 1
             prob_ph(iph) = prob_ph(iph) + gs_weight
             dens_ph = dens_ph + (iph-1)*gs_weight

             !<X> and <X^2> with X=(b+bdg)/sqrt(2)
             if(iph<DimPh)then
                j = i_el + (iph)*sectorI%DimEl !bdg
                X_ph = X_ph + sqrt(dble(iph)/2.d0)*(state_dvec(i)*state_dvec(j))*peso
             end if
             if(iph>1)then
                j = i_el + (iph-2)*sectorI%DimEl !b
                X_ph = X_ph + sqrt(dble(iph-1)/2.d0)*(state_dvec(i)*state_dvec(j))*peso
             end if
             !
             X2_ph = X2_ph + 0.5d0*(1+2*(iph-1))*gs_weight
             if(iph<DimPh-1)then
                j= i_el + (iph+1)*sectorI%DimEl
                X2_ph = X2_ph + 0.5d0*sqrt(dble((iph)*(iph+1)))*(state_dvec(i)*state_dvec(j))*peso
             end if
             if(iph>2)then
                j= i_el + (iph-3)*sectorI%DimEl
                X2_ph = X2_ph + 0.5d0*sqrt(dble((iph-2)*(iph-1)))*(state_dvec(i)*state_dvec(j))*peso
             end if
             !
             !compute the lattice probability distribution function
             if(Dimph>1 .AND. iph==1) then
                val = 1
                !val = 1 + Nr. of polarized orbitals (full or empty) makes sense only for 2 orbs
                do iorb=1,Norb
                   val = val + abs(nint(sign((nt(iorb) - 1.d0),real(g_ph(iorb,iorb)))))
                enddo
                call prob_distr_ph(state_dvec,val)
             end if
          enddo
          !
          call delete_sector(sectorI)
       endif
       !
       if(allocated(state_dvec))deallocate(state_dvec)
       !
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif

    !EVALUATE EXCITON OP <S_ab> AND <T^z_ab>
    !<S_ab>  :=   <C^+_{a,up}C_{b,up} + C^+_{a,dw}C_{b,dw}>
    !<T^z_ab>:=   <C^+_{a,up}C_{b,up} - C^+_{a,dw}C_{b,dw}>
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_normal: eval exciton OP Singlet, Triplet_Z"
#endif
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_normal: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_dvector(MpiComm,state_list,istate,state_dvec) 
       else
          call es_return_dvector(state_list,istate,state_dvec) 
       endif
#else
       call es_return_dvector(state_list,istate,state_dvec) 
#endif

       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(Mpimaster)call build_sector(isector,sectorI)
       !    
       do iorb=1,Norb
          do jorb=iorb+1,Norb
             !
             !\Theta_upup = <v|v>, |v> = (C_aup + C_bup)|>
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,1,sectorI,sectorJ) !c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_dvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !+c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !
                   theta_upup(iorb,jorb) = theta_upup(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
             !\Theta_dwdw = <v|v>, |v> = (C_adw + C_bdw)|>
             jsector = getCsector(1,2,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,2,sectorI,sectorJ) !c_b,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = sgn*state_dvec(i)
                   enddo
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !+c_a,dw
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   call delete_sector(sectorJ)
                   !
                   theta_dwdw(iorb,jorb) = theta_dwdw(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
          enddo
       enddo
       !
       ! Compute < \Delta_exc^2 >
       do iorb=1,Norb
          do jorb=iorb+1,Norb
             allocate(vupinit(sectorI%Dim));vupinit=zero
             allocate(vdwinit(sectorI%Dim));vdwinit=zero
             !
             ! spin up
             jsector = getCsector(1,1,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim));vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,1,sectorI,sectorJ) !c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   do j=1,sectorJ%Dim
                      call apply_op_CDG(j,i,sgn,iorb,1,1,sectorJ,sectorI) !cdg_a,up
                      if(sgn==0d0.OR.i==0)cycle
                      vupinit(i) = vupinit(i) + sgn*vvinit(j) ! ( cdg_a,up c_b,up ) |v> 
                   enddo
                   vvinit=0.d0
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,1,sectorI,sectorJ) !c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   do j=1,sectorJ%Dim
                      call apply_op_CDG(j,i,sgn,jorb,1,1,sectorJ,sectorI) !cdg_b,up
                      if(sgn==0d0.OR.i==0)cycle
                      vupinit(i) = vupinit(i) + sgn*vvinit(j) ! ( cdg_a,up c_b,up + cdg_b,up c_a,up) |v>
                   enddo
                   call delete_sector(sectorJ)
                   !
                   if(allocated(vvinit))deallocate(vvinit)
                endif
             endif
             !
             ! spin down
             jsector = getCsector(1,2,isector)
             if(jsector/=0)then
                if(Mpimaster)then
                   call build_sector(jsector,sectorJ)
                   allocate(vvinit(sectorJ%Dim))
                   vvinit=zero
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,jorb,1,2,sectorI,sectorJ) !c_b,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   do j=1,sectorJ%Dim
                      call apply_op_CDG(j,i,sgn,iorb,1,2,sectorJ,sectorI) !cdg_a,up
                      if(sgn==0d0.OR.i==0)cycle
                      vdwinit(i) = vdwinit(i) + sgn*vvinit(j) ! ( cdg_a,up c_b,up ) |v> 
                   enddo
                   vvinit=0.d0
                   do i=1,sectorI%Dim
                      call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !c_a,up
                      if(sgn==0d0.OR.j==0)cycle
                      vvinit(j) = vvinit(j) + sgn*state_dvec(i)
                   enddo
                   do j=1,sectorJ%Dim
                      call apply_op_CDG(j,i,sgn,jorb,1,2,sectorJ,sectorI) !cdg_b,up
                      if(sgn==0d0.OR.i==0)cycle
                      vdwinit(i) = vdwinit(i) + sgn*vvinit(j) ! ( cdg_a,up c_b,up + cdg_b,up c_a,up) |v>
                   enddo
                   call delete_sector(sectorJ)
                   !
                   if(allocated(vvinit))deallocate(vvinit)
                   exct_s02(iorb,jorb) = exct_s02(iorb,jorb) + 0.25d0*dot_product(vupinit+vdwinit,vupinit+vdwinit)*peso
                   exct_tz2(iorb,jorb) = exct_tz2(iorb,jorb) + 0.25d0*dot_product(vupinit-vdwinit,vupinit-vdwinit)*peso
                endif
             endif
             !
             if(allocated(vupinit))deallocate(vupinit)
             if(allocated(vdwinit))deallocate(vdwinit)      
             !
          enddo
       enddo
       !
       if(allocated(state_dvec))deallocate(state_dvec)
       !
    enddo
    !
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          exct_s0(iorb,jorb) = 0.5d0*(theta_upup(iorb,jorb) + theta_dwdw(iorb,jorb) - dens(iorb) - dens(jorb))
          exct_tz(iorb,jorb) = 0.5d0*(theta_upup(iorb,jorb) - theta_dwdw(iorb,jorb) - magZ(iorb) - magZ(jorb))
       enddo
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif

    !
    !IMPURITY DENSITY MATRIX
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_normal: eval impurity density matrix <C^+_a C_b>"
#endif
    if(allocated(imp_density_matrix)) deallocate(imp_density_matrix)
    allocate(imp_density_matrix(Nspin,Nspin,Norb,Norb));imp_density_matrix=zero
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_normal: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_dvector(MpiComm,state_list,istate,state_dvec) 
       else
          call es_return_dvector(state_list,istate,state_dvec) 
       endif
#else
       call es_return_dvector(state_list,istate,state_dvec) 
#endif

       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       if(MpiMaster)then
          call build_sector(isector,sectorI)
          do i=1,sectorI%Dim
             iph = (i-1)/(sectorI%DimEl) + 1
             i_el = mod(i-1,sectorI%DimEl) + 1
             call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
             !
             call build_op_Ns(i,IbUp,IbDw,sectorI)
             Nud(1,:)=IbUp
             Nud(2,:)=IbDw
             !
             !Diagonal densities
             do ispin=1,Nspin
                do iorb=1,Norb
                   imp_density_matrix(ispin,ispin,iorb,iorb) = &
                        imp_density_matrix(ispin,ispin,iorb,iorb) + &
                        peso*nud(ispin,iorb)*(state_dvec(i))*state_dvec(i)
                enddo
             enddo
             !
             !Off-diagonal
             if(ed_total_ud)then
                do ispin=1,Nspin
                   do iorb=1,Norb
                      do jorb=1,Norb
                         !
                         if((Nud(ispin,jorb)==1).and.(Nud(ispin,iorb)==0))then
                            iud(1) = sectorI%H(1)%map(Indices(1))
                            iud(2) = sectorI%H(2)%map(Indices(2))
                            call c(jorb,iud(ispin),r,sgn1)
                            call cdg(iorb,r,k,sgn2)
                            Jndices = Indices
                            Jndices(1+(ispin-1)*Ns_Ud) = &
                                 binary_search(sectorI%H(1+(ispin-1)*Ns_Ud)%map,k)
                            call indices2state(Jndices,[sectorI%DimUps,sectorI%DimDws],j)
                            !
                            j = j + (iph-1)*sectorI%DimEl
                            !
                            imp_density_matrix(ispin,ispin,iorb,jorb) = &
                                 imp_density_matrix(ispin,ispin,iorb,jorb) + &
                                 peso*sgn1*state_dvec(i)*sgn2*(state_dvec(j))
                         endif
                      enddo
                   enddo
                enddo
             endif
             !
             !
          enddo
          call delete_sector(sectorI)         
       endif
       !
       if(allocated(state_dvec))deallocate(state_dvec)
       !
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif
    !
    !
    !
    !
    if(MPIMASTER)then
       call get_szr
       if(DimPh>1) w_ph = sqrt(-2.d0*w0_ph/impDmats_ph(0)) !renormalized phonon frequency
       if(iolegend)call write_legend
       call write_observables()
       write(LOGfile,"(A,10f18.12,f18.12)")&
            " dens"//reg(ed_file_suffix)//"=",(dens(iorb),iorb=1,Norb),sum(dens)
       write(LOGfile,"(A,10f18.12)")&
            " docc"//reg(ed_file_suffix)//"=",(docc(iorb),iorb=1,Norb)
       if(any(exct_S0/=0d0))write(LOGfile,"(A,10f18.12)")&
            "excS0"//reg(ed_file_suffix)//"=",((exct_S0(iorb,jorb),iorb=1,Norb),jorb=1,Norb)
       if(any(exct_tz/=0d0))write(LOGfile,"(A,10f18.12)")&
            "excTZ"//reg(ed_file_suffix)//"=",((exct_Tz(iorb,jorb),iorb=1,Norb),jorb=1,Norb)
       if(any(exct_S0/=0d0))write(LOGfile,"(A,10f18.12)")&
            "excS02"//reg(ed_file_suffix)//"=",((exct_S02(iorb,jorb),iorb=1,Norb),jorb=1,Norb)
       if(any(exct_tz/=0d0))write(LOGfile,"(A,10f18.12)")&
            "excTZ2"//reg(ed_file_suffix)//"=",((exct_Tz2(iorb,jorb),iorb=1,Norb),jorb=1,Norb)
       
       if(Nspin==2)then
          write(LOGfile,"(A,10f18.12,A)")&
               " magZ"//reg(ed_file_suffix)//"=",(magz(iorb),iorb=1,Norb)
       endif
       if(DimPh>1)call write_pdf()
       !
       do iorb=1,Norb
          ed_dens_up(iorb)=dens_up(iorb)
          ed_dens_dw(iorb)=dens_dw(iorb)
          ed_dens(iorb)   =dens(iorb)
          ed_docc(iorb)   =docc(iorb)
       enddo
    endif
#ifdef _MPI
    if(MpiStatus)then
       call Bcast_MPI(MpiComm,ed_dens_up)
       call Bcast_MPI(MpiComm,ed_dens_dw)
       call Bcast_MPI(MpiComm,ed_dens)
       call Bcast_MPI(MpiComm,ed_docc)
    endif
#endif
    !
    deallocate(dens,docc,dens_up,dens_dw,magz,sz2,n2,Prob)
    deallocate(exct_S0,exct_Tz,exct_s02,exct_tz2)
    deallocate(simp,zimp,prob_ph,pdf_ph,pdf_part)
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif
  end subroutine observables_normal





  !+------------------------------------------------------------------+
  !PURPOSE  : Get internal energy from the Impurity problem.
  !+-------------------------------------------------------------------+
  subroutine local_energy_normal()
    integer                             :: istate,iud(2),jud(2)
    integer,dimension(2*Ns_Ud)          :: Indices,Jndices
    integer,dimension(Ns_Ud,Ns_Orb)     :: Nups,Ndws  ![1,Ns]-[Norb,1+Nbath]
    real(8),dimension(Ns)               :: Nup,Ndw
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG local_energy_normal"
#endif
    Egs     = state_list%emin
    ed_Ehartree= 0.d0
    ed_Eknot   = 0.d0
    ed_Epot    = 0.d0
    ed_Eeph    = 0.d0
    ed_Dust    = 0.d0
    ed_Dund    = 0.d0
    ed_Dse     = 0.d0
    ed_Dph     = 0.d0
    !
    !
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG local_energy_normal: get contribution from state:"//str(istate)
#endif
#ifdef _MPI
       if(MpiStatus)then
          call es_return_dvector(MpiComm,state_list,istate,state_dvec) 
       else
          call es_return_dvector(state_list,istate,state_dvec) 
       endif
#else
       call es_return_dvector(state_list,istate,state_dvec) 
#endif
       !
       peso = 1.d0 ; if(finiteT)peso=exp(-beta*(Ei-Egs))
       peso = peso/zeta_function
       !
       !Master:
       if(MpiMaster)then
          call build_sector(isector,sectorI)
          do i=1,sectorI%Dim
             iph = (i-1)/(sectorI%DimEl) + 1
             i_el = mod(i-1,sectorI%DimEl) + 1
             !
             call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
             do ii=1,Ns_Ud
                mup = sectorI%H(ii)%map(Indices(ii))
                mdw = sectorI%H(ii+Ns_Ud)%map(Indices(ii+Ns_ud))
                Nups(ii,:) = Bdecomp(mup,Ns_Orb) ![Norb,1+Nbath]
                Ndws(ii,:) = Bdecomp(mdw,Ns_Orb)
             enddo
             Nup = Breorder(Nups)
             Ndw = Breorder(Ndws)
             !
             gs_weight=peso*abs(state_dvec(i))**2
             !
             !> H_Imp: Diagonal Elements, i.e. local part
             do iorb=1,Norb
                ed_Eknot = ed_Eknot + impHloc(1,1,iorb,iorb)*Nup(iorb)*gs_weight
                ed_Eknot = ed_Eknot + impHloc(Nspin,Nspin,iorb,iorb)*Ndw(iorb)*gs_weight
             enddo
             ! !> H_imp: Off-diagonal elements, i.e. non-local part. 
             if(ed_total_ud)then
                iup = Indices(1)
                idw = Indices(2)
                mup = sectorI%H(1)%map(iup)
                mdw = sectorI%H(2)%map(idw)
                do iorb=1,Norb
                   do jorb=1,Norb
                      !UP
                      Jcondition = &
                           (impHloc(1,1,iorb,jorb)/=0d0) .AND. &
                           (Nup(jorb)==1) .AND. (Nup(iorb)==0)
                      if (Jcondition) then
                         call c(jorb,mup,k1,sg1)
                         call cdg(iorb,k1,k2,sg2)
                         jup = binary_search(sectorI%H(1)%map,k2)
                         j   = jup + (idw-1)*sectorI%DimUp
                         ed_Eknot = ed_Eknot + &
                              impHloc(1,1,iorb,jorb)*sg1*sg2*state_dvec(i)*(state_dvec(j))*peso
                      endif
                      !
                      !DW
                      Jcondition = &
                           (impHloc(Nspin,Nspin,iorb,jorb)/=0d0) .AND. &
                           (ndw(jorb)==1) .AND. (ndw(iorb)==0)
                      if (Jcondition) then
                         call c(jorb,mdw,k1,sg1)
                         call cdg(iorb,k1,k2,sg2)
                         jdw = binary_search(sectorI%H(2)%map,k2)
                         j   = iup + (jdw-1)*sectorI%DimUp
                         ed_Eknot = ed_Eknot + &
                              impHloc(Nspin,Nspin,iorb,jorb)*sg1*sg2*state_dvec(i)*(state_dvec(j))*peso
                      endif
                   enddo
                enddo
                ! 
                !SPIN-EXCHANGE Jx
                if(Jhflag.AND.Jx/=0d0)then
                   do iorb=1,Norb
                      do jorb=1,Norb
                         Jcondition=(&
                              (iorb/=jorb).AND.&
                              (nup(jorb)==1).AND.&
                              (ndw(iorb)==1).AND.&
                              (ndw(jorb)==0).AND.&
                              (nup(iorb)==0))
                         if(Jcondition)then
                            call c(iorb,mdw,k1,sg1)  !DW
                            call cdg(jorb,k1,k2,sg2) !DW
                            jdw=binary_search(sectorI%H(2)%map,k2)
                            call c(jorb,mup,k3,sg3)  !UP
                            call cdg(iorb,k3,k4,sg4) !UP
                            jup=binary_search(sectorI%H(1)%map,k4)
                            j = jup + (jdw-1)*sectorI%DimUp
                            !
                            ed_Epot = ed_Epot + Jx*sg1*sg2*sg3*sg4*state_dvec(i)*state_dvec(j)*peso
                            ed_Dse = ed_Dse + sg1*sg2*sg3*sg4*state_dvec(i)*state_dvec(j)*peso
                            !
                         endif
                      enddo
                   enddo
                endif
                !
                ! PAIR-HOPPING Jp
                if(Jhflag.AND.Jp/=0d0)then
                   do iorb=1,Norb
                      do jorb=1,Norb
                         Jcondition=(&
                              (nup(jorb)==1).AND.&
                              (ndw(jorb)==1).AND.&
                              (ndw(iorb)==0).AND.&
                              (nup(iorb)==0))
                         if(Jcondition)then
                            call c(jorb,mdw,k1,sg1)       !c_jorb_dw
                            call cdg(iorb,k1,k2,sg2)      !c^+_iorb_dw
                            jdw = binary_search(sectorI%H(2)%map,k2)
                            call c(jorb,mup,k3,sg3)       !c_jorb_up
                            call cdg(iorb,k3,k4,sg4)      !c^+_iorb_up
                            jup = binary_search(sectorI%H(1)%map,k4)
                            j = jup + (jdw-1)*sectorI%DimUp
                            !
                            ed_Epot = ed_Epot + Jp*sg1*sg2*sg3*sg4*state_dvec(i)*state_dvec(j)*peso
                            ed_Dph = ed_Dph + sg1*sg2*sg3*sg4*state_dvec(i)*state_dvec(j)*peso
                            !
                         endif
                      enddo
                   enddo
                endif
             endif
             !
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
             !
             !ELECTRON-PHONON COUPLING
             if(nph>0)then
                !UP-SPIN
                do iorb=1,Norb
                   !DIAGONAL
                   if(g_ph(iorb,iorb)/=zero)then
                      if(iph<DimPh)then !bdg
                         j = i_el + (iph  )*sectorI%DimEl
                         ed_Eeph = ed_Eeph + g_ph(iorb,iorb)*(Nup(iorb)-0.5d0)*sqrt(dble(iph))*state_dvec(i)*state_dvec(j)
                      endif
                      if(iph>1)then !b
                         j = i_el + (iph -2 )*sectorI%DimEl
                         ed_Eeph = ed_Eeph + g_ph(iorb,iorb)*(Nup(iorb)-0.5d0)*sqrt(dble(iph-1))*state_dvec(i)*state_dvec(j)
                      endif
                   end if
                   !OFF-DIAGONAL
                   do jorb=1,Norb
                      if(g_ph(iorb,jorb)/=zero .and.&
                           Nup(jorb)==1 .and. Nup(iorb)==0 )then
                         call c(jorb,mup,k1,sg1)
                         call cdg(iorb,k1,k2,sg2)
                         jup  = binary_search(sectorI%H(1)%map,k2)
                         jdw  = idw
                         j_el    = jup + (jdw-1)*sectorI%DimUp                         
                         if(iph<DimPh)then !bdg
                            j = j_el + (iph  )*sectorI%DimEl
                            ed_Eeph = ed_Eeph + g_ph(iorb,jorb)*sqrt(dble(iph))*state_dvec(i)*state_dvec(j)*sg1*sg2
                         endif
                         if(iph>1)then !b
                            j = j_el + (iph -2 )*sectorI%DimEl
                            ed_Eeph = ed_Eeph + g_ph(iorb,jorb)*sqrt(dble(iph-1))*state_dvec(i)*state_dvec(j)*sg1*sg2
                         endif
                      endif
                   enddo
                end do
                !DW-SPIN
                do iorb=1,Norb
                   !DIAGONAL
                   if(g_ph(iorb,iorb)/=zero)then
                      if(iph<DimPh)then !bdg
                         j = i_el + (iph  )*sectorI%DimEl
                         ed_Eeph = ed_Eeph + g_ph(iorb,iorb)*(Ndw(iorb)-0.5d0)*sqrt(dble(iph))*state_dvec(i)*state_dvec(j)
                      endif
                      if(iph>1)then!b
                         j = i_el + (iph -2 )*sectorI%DimEl
                         ed_Eeph = ed_Eeph + g_ph(iorb,iorb)*(Ndw(iorb)-0.5d0)*sqrt(dble(iph-1))*state_dvec(i)*state_dvec(j)
                      endif
                   end if
                   do jorb=1,Norb
                      !OFF-DIAGONAL
                      if(g_ph(iorb,jorb)/=zero .and.&
                           Ndw(jorb)==1 .and. Ndw(iorb)==0 )then
                         call c(jorb,mdw,k1,sg1)
                         call cdg(iorb,k1,k2,sg2)
                         jdw  = binary_search(sectorI%H(2)%map,k2)
                         jup  = iup
                         j_el    = jup + (jdw-1)*sectorI%DimUp                         
                         if(iph<DimPh)then !bdg
                            j = j_el + (iph  )*sectorI%DimEl
                            ed_Eeph = ed_Eeph + g_ph(iorb,jorb)*sqrt(dble(iph))*state_dvec(i)*state_dvec(j)*sg1*sg2
                         endif
                         if(iph>1)then !b
                            j = j_el + (iph -2 )*sectorI%DimEl
                            ed_Eeph = ed_Eeph + g_ph(iorb,jorb)*sqrt(dble(iph-1))*state_dvec(i)*state_dvec(j)*sg1*sg2
                         endif
                      endif
                   enddo
                enddo
             endif
          enddo
          call delete_sector(sectorI)         
       endif
       !
       if(allocated(state_dvec))deallocate(state_dvec)
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
       call Bcast_MPI(MpiComm,ed_Eeph)
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
       write(LOGfile,"(A,10f18.12)")"Eeph    =",ed_Eeph
    endif
    !
    if(MPIMASTER)then
       call write_energy_info()
       call write_energy()
    endif
    !
    !
  end subroutine local_energy_normal








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
    integer :: unit,iorb,jorb,ispin,stride
    
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
    write(unit,"(A)") "# imp_last.ed"
    write(unit,"(A1,90(A10,6X))") "#", "1s2tot", "2egs", "3nph", "4w_ph","5X_ph", "6X2_ph"
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# exciton_last.ed"
    write(unit,"(A1,90(A10,6X))") "#","1S_0" , "2T_z"
    write(unit,"(A1,90(A10,6X))") "# *****"
    write(unit,"(A1,90(A10,6X))") "# exc_square_last.ed"
    write(unit,"(A1,90(A10,6X))") "#","1S_02" , "2T_z2"
    close(unit)
    !
    unit = free_unit()
    open(unit,file="parameters_info.ed")
    write(unit,"(A1,90(A14,1X))")"#","1xmu","2beta",&
         (reg(txtfy(2+iorb))//"U_"//reg(txtfy(iorb)),iorb=1,Norb),&
         reg(txtfy(2+Norb+1))//"U'",reg(txtfy(2+Norb+2))//"Jh"
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Nph_probability_info.ed")
    write(unit,"(A1,90(A10,6X))")"#",&
         (reg(txtfy(i+1))//"Nph="//reg(txtfy(i)),i=0,DimPh-1)
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
         reg(txtfy(8))//"<Dph>",&
         reg(txtfy(9))//"<Eeph>"
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
    unit = free_unit()
    open(unit,file="imp_all"//reg(ed_file_suffix)//".ed",position='append')
    write(unit,"(90(F15.9,1X))") s2tot, egs, dens_ph, w_ph, X_ph, X2_ph
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exciton_all"//reg(ed_file_suffix)//".ed",position='append')
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")exct_s0(iorb,jorb),exct_tz(iorb,jorb)
       enddo
    enddo
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exc_square_all"//reg(ed_file_suffix)//".ed",position='append')
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")exct_s02(iorb,jorb),exct_tz2(iorb,jorb)
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
    unit = free_unit()
    open(unit,file="imp_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") s2tot, egs, dens_ph, w_ph, X_ph, X2_ph
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exciton_last"//reg(ed_file_suffix)//".ed")
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")&
               exct_s0(iorb,jorb),exct_tz(iorb,jorb)
       enddo
    enddo
    close(unit)
    !
    unit = free_unit()
    open(unit,file="exc_square_last"//reg(ed_file_suffix)//".ed")
    do iorb=1,Norb
       do jorb=iorb+1,Norb
          write(unit,"(90(F15.9,1X))")&
               exct_s02(iorb,jorb),exct_tz2(iorb,jorb)
       enddo
    enddo
    close(unit)

    !
    close(unit)
    unit = free_unit()
    open(unit,file="parameters_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90F15.9)")xmu,beta,(uloc(iorb),iorb=1,Norb),Ust,Jh,Jx,Jp
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Occupation_prob"//reg(ed_file_suffix)//".ed")
    write(unit,"(125F15.9)")Uloc(1),Prob,sum(Prob)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Nph_probability"//reg(ed_file_suffix)//".ed")
    write(unit,"(90(F15.9,1X))") (prob_ph(i),i=1,DimPh)
    close(unit)
    !
  end subroutine write_observables

  subroutine write_energy()
    integer :: unit
    unit = free_unit()
    open(unit,file="energy_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90F15.9)")ed_Epot,ed_Epot-ed_Ehartree,ed_Eknot,ed_Ehartree,ed_Dust,ed_Dund,ed_Dse,ed_Dph,ed_Eeph
    close(unit)
  end subroutine write_energy

  subroutine write_pdf()
    integer :: unit,i
    real(8) :: x,dx
    unit = free_unit()
    open(unit,file="lattice_prob"//reg(ed_file_suffix)//".ed")
    dx = (xmax-xmin)/dble(Lpos)
    x = xmin
    do i=1,Lpos
       write(unit,"(5F15.9)") x,pdf_ph(i),pdf_part(i,:)
       x = x + dx
    enddo
    close(unit)
  end subroutine write_pdf



  !+-------------------------------------------------------------------+
  !PURPOSE  : subroutines useful for the phonons
  !+-------------------------------------------------------------------+

  !Compute the local lattice probability distribution function (PDF), i.e. the local probability of displacement
  !as a function of the displacement itself
  subroutine prob_distr_ph(vec,val)
    real(8),dimension(:) :: vec
    real(8)              :: psi(0:DimPh-1)
    real(8)              :: x,dx
    integer              :: i,j,i_ph,j_ph,val
    integer              :: istart,jstart,iend,jend
    !
    dx = (xmax-xmin)/dble(Lpos)
    !
    x = xmin
    do i=1,Lpos
       call Hermite(x,psi)
       !
       do i_ph=1,DimPh
          istart = i_el + (i_ph-1)*sectorI%DimEl
          !
          do j_ph=1,DimPh
             jstart = i_el + (j_ph-1)*sectorI%DimEl
             !
             pdf_ph(i) = pdf_ph(i) + peso*psi(i_ph-1)*psi(j_ph-1)*vec(istart)*vec(jstart)
             ! if(ph_type==1 .and. Norb==2 .and. val<4) then	!all this conditions should disappear soon or later...
             pdf_part(i,val) = pdf_part(i,val) + peso*psi(i_ph-1)*psi(j_ph-1)*vec(istart)*vec(jstart)
             ! endif
          enddo
       enddo
       !
       x = x + dx
    enddo
  end subroutine prob_distr_ph

  !Compute the Hermite functions (i.e. harmonic oscillator eigenfunctions)
  !the output is a vector with the functions up to order Dimph-1 evaluated at position x
  subroutine Hermite(x,psi)
    real(8),intent(in)  ::  x
    real(8),intent(out) ::  psi(0:DimPh-1)
    integer             ::  i
    real(8)             ::  den
    !
    den=1.331335373062335d0!pigr**(0.25d0)
    !
    psi(0)=exp(-0.5d0*x*x)/den
    psi(1)=exp(-0.5d0*x*x)*sqrt(2d0)*x/den
    !
    do i=2,DimPh-1
       psi(i)=2*x*psi(i-1)/sqrt(dble(2*i))-psi(i-2)*sqrt(dble(i-1)/dble(i))
    enddo
  end subroutine Hermite


end MODULE ED_OBSERVABLES_NORMAL

















