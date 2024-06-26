MODULE ED_OBSERVABLES_SUPERC
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
  USE ED_HAMILTONIAN_SUPERC
  !
  implicit none
  private
  !
  public :: observables_superc
  public :: local_energy_superc


  logical,save                        :: iolegend=.true.
  real(8),dimension(:),allocatable    :: dens,dens_up,dens_dw
  real(8),dimension(:),allocatable    :: docc
  real(8),dimension(:),allocatable    :: magZ,magX,magY
  real(8),dimension(:,:),allocatable  :: phiscAB
  real(8),dimension(:),allocatable    :: phisc
  real(8),dimension(:,:),allocatable  :: sz2,n2
  real(8),dimensioN(:,:),allocatable  :: zimp,simp
  real(8)                             :: s2tot
  real(8)                             :: Egs
  real(8)                             :: Ei
  real(8),dimension(:),allocatable    :: Prob
  real(8),dimension(:),allocatable    :: prob_ph
  real(8),dimension(:),allocatable    :: pdf_ph
  real(8),dimension(:,:),allocatable  :: pdf_part
  real(8)                             :: dens_ph
  real(8)                             :: X_ph, X2_ph
  real(8)                             :: w_ph
  !
  integer                             :: iorb,jorb,istate
  integer                             :: ispin,jspin
  integer                             :: isite,jsite
  integer                             :: ibath
  integer                             :: r,m,k,k1,k2,k3,k4
  integer                             :: iup,idw
  integer                             :: jup,jdw
  integer                             :: mup,mdw
  integer                             :: iph,i_el,j_el,isz
  real(8)                             :: sgn,sgn1,sgn2,sg1,sg2,sg3,sg4
  real(8)                             :: gs_weight
  !
  real(8)                             :: peso
  real(8)                             :: norm
  !
  integer                             :: i,j,ii,iprob
  integer                             :: isector,jsector
  !
  complex(8),dimension(:),allocatable :: vvinit
  complex(8),dimension(:),allocatable :: state_cvec
  logical                             :: Jcondition
  !
  type(sector)                        :: sectorI,sectorJ


contains 



  !+-------------------------------------------------------------------+
  !PURPOSE  : Evaluate and print out many interesting physical qties
  !+-------------------------------------------------------------------+
  subroutine observables_superc()
    integer                 :: val
    integer,dimension(2*Ns) :: ib
    integer,dimension(2,Ns) :: Nud
    integer,dimension(Ns)   :: IbUp,IbDw
    real(8),dimension(Norb) :: nup,ndw,Sz,nt
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG observables_superc"
#endif
    !
    allocate(dens(Norb),dens_up(Norb),dens_dw(Norb))
    allocate(docc(Norb))
    allocate(magz(Norb),sz2(Norb,Norb),n2(Norb,Norb))
    allocate(phisc(Norb),phiscAB(Norb,Norb))
    allocate(simp(Norb,Nspin),zimp(Norb,Nspin))
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
    phisc   = 0.d0
    phiscAB = 0.d0
    magz    = 0.d0
    sz2     = 0.d0
    n2      = 0.d0
    s2tot   = 0.d0
    Prob    = 0.d0
    prob_ph = 0.d0
    X_ph = 0.d0
    X2_ph = 0.d0
    dens_ph = 0.d0
    pdf_ph  = 0.d0
    pdf_part= 0.d0
    w_ph    = w0_ph
    !
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")&
         "DEBUG observables_superc: get local observables"
#endif
    do istate=1,state_list%size
       isector = es_return_sector(state_list,istate)
       Ei      = es_return_energy(state_list,istate)
       !
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG observables_superc: get contribution from state:"//str(istate)
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
             i_el = mod(i-1,sectorI%DimEl)+1
             m    = sectorI%H(1)%map(i_el)
             ib   = bdecomp(m,2*Ns)
             do iorb=1,Norb
                nup(iorb)= dble(ib(iorb))
                ndw(iorb)= dble(ib(iorb+Ns))
             enddo
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
             !
             !<X> and <X^2> with X=(b+bdg)/sqrt(2)
             if(iph<DimPh)then
                j= i_el + (iph)*sectorI%DimEl
                X_ph = X_ph + sqrt(2.d0*dble(iph))*real(state_cvec(i)*conjg(state_cvec(j)))*peso
             end if
             X2_ph = X2_ph + 0.5d0*(1+2*(iph-1))*gs_weight
             if(iph<DimPh-1)then
                j= i_el + (iph+1)*sectorI%DimEl
                X2_ph = X2_ph + sqrt(dble((iph)*(iph+1)))*real(state_cvec(i)*conjg(state_cvec(j)))*peso
             end if
             !compute the lattice probability distribution function
             if(Dimph>1 .AND. iph==1) then
                val = 1
                !val = 1 + Nr. of polarized orbitals (full or empty) makes sense only for 2 orbs
                do iorb=1,Norb
                   val = val + abs(nint(sign((nt(iorb) - 1.d0),real(g_ph(iorb,iorb)))))
                enddo
                call prob_distr_ph(state_cvec,val)
             end if
          enddo
          !
          !
          !SUPERCONDUCTING ORDER PARAMETER
#ifdef _DEBUG
          if(ed_verbose>2)write(Logfile,"(A)")"DEBUG observables_superc: get OP"
#endif
          do ispin=1,Nspin 
             !GET <(b_up + adg_dw)(bdg_up + a_dw)> = 
             !<b_up*bdg_up> + <adg_dw*a_dw> + <b_up*a_dw> + <adg_dw*bdg_up> = 
             !<n_a,dw> + < 1 - n_b,up> + 2*<PHI>_ab
             do iorb=1,Norb !A
                do jorb=1,Norb !B
                   isz = getsz(isector)
                   if(isz<Ns)then
                      jsector = getsector(isz+1,1)
                      call build_sector(jsector,sectorJ)
                      allocate(vvinit(sectorJ%Dim));vvinit=zero
                      do i=1,sectorI%Dim
                         call apply_op_CDG(i,j,sgn,jorb,1,1,sectorI,sectorJ)!bdg_up
                         if(sgn==0d0.OR.j==0)cycle
                         vvinit(j) = sgn*state_cvec(i)
                      enddo
                      do i=1,sectorI%Dim
                         call apply_op_C(i,j,sgn,iorb,1,2,sectorI,sectorJ) !a_dw
                         if(sgn==0d0.OR.j==0)cycle
                         vvinit(j) = vvinit(j) + sgn*state_cvec(i)
                      enddo
                      call delete_sector(sectorJ)
                      phiscAB(iorb,jorb) = phiscAB(iorb,jorb) + dot_product(vvinit,vvinit)*peso
                   endif
                   if(allocated(vvinit))deallocate(vvinit)
                enddo
             enddo
          enddo
          !
          call delete_sector(sectorI)
          !
       endif
       !
       if(allocated(state_cvec))deallocate(state_cvec)
       !
    enddo
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif
    !
    do iorb=1,Norb
       do jorb=1,Norb
          phiscAB(iorb,jorb) = 0.5d0*(phiscAB(iorb,jorb) - dens_dw(iorb) - (1.d0-dens_up(jorb)))
       enddo
       phisc(iorb)=phiscAB(iorb,iorb)
    enddo
    !
    !
    if(MPIMASTER)then
       call get_szr
       if(DimPh>1) w_ph = sqrt(-2.d0*w0_ph/impDmats_ph(0)) !renormalized phonon frequency
       if(iolegend)call write_legend
       call write_observables()
    endif
    write(LOGfile,"(A,10f18.12,f18.12,A)")"dens"//reg(ed_file_suffix)//"=",(dens(iorb),iorb=1,Norb),sum(dens)
    write(LOGfile,"(A,10f18.12,A)")    "docc"//reg(ed_file_suffix)//"=",(docc(iorb),iorb=1,Norb)
    write(LOGfile,"(A,20f18.12,A)")    "phiAB "//reg(ed_file_suffix)//"=",((phiscAB(iorb,jorb),iorb=1,Norb),jorb=1,Norb)
    write(LOGfile,"(A,20f18.12,A)")     " | phiAA*Uloc ",(abs(uloc(iorb))*phisc(iorb),iorb=1,Norb)
    if(Nspin==2)then
       write(LOGfile,"(A,10f18.12,A)")    "magZ"//reg(ed_file_suffix)//"=",(magz(iorb),iorb=1,Norb)
    endif
    !
    if(DimPh>1)call write_pdf()
    !
    do iorb=1,Norb
       ed_dens_up(iorb)=dens_up(iorb)
       ed_dens_dw(iorb)=dens_dw(iorb)
       ed_dens(iorb)   =dens(iorb)
       ed_docc(iorb)   =docc(iorb)
       ed_mag(1,iorb)  =magZ(iorb)
       ed_phisc(iorb)  =phisc(iorb)
    enddo
#ifdef _MPI
    if(MpiStatus)then
       call Bcast_MPI(MpiComm,ed_dens_up)
       call Bcast_MPI(MpiComm,ed_dens_dw)
       call Bcast_MPI(MpiComm,ed_dens)
       call Bcast_MPI(MpiComm,ed_docc)
       call Bcast_MPI(MpiComm,ed_phisc)
       call Bcast_MPI(MpiComm,ed_mag)
       if(allocated(imp_density_matrix))call Bcast_MPI(MpiComm,imp_density_matrix)
    endif
#endif
    !
    deallocate(dens,docc,phiscAB,phisc,dens_up,dens_dw,magz,sz2,n2,Prob)
    deallocate(simp,zimp,prob_ph,pdf_ph,pdf_part)
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")""
#endif
  end subroutine observables_superc







  !+-------------------------------------------------------------------+
  !PURPOSE  : Get internal energy from the Impurity problem.
  !+-------------------------------------------------------------------+
  subroutine local_energy_superc()
    integer,dimension(2*Ns) :: ib
    integer,dimension(2,Ns) :: Nud
    integer,dimension(Ns)   :: IbUp,IbDw
    real(8),dimension(Norb)         :: nup,ndw
    real(8),dimension(Nspin,Norb)   :: eloc
    !
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG local_energy_superc"
#endif
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
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A)")&
            "DEBUG local_energy_superc: get contribution from state:"//str(istate)
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
             iph  = (i-1)/(sectorI%DimEl)+1
             i_el = mod(i-1,sectorI%DimEl)+1
             m    = sectorI%H(1)%map(i_el)
             ib   = bdecomp(m,2*Ns)
             do iorb=1,Norb
                nup(iorb)=dble(ib(iorb))
                ndw(iorb)=dble(ib(iorb+Ns))
             enddo
             !
             gs_weight=peso*abs(state_cvec(i))**2
             !
             !start evaluating the Tr(H_loc) to estimate potential energy
             !> H_Imp: Diagonal Elements, i.e. local part
             do iorb=1,Norb
                ed_Eknot = ed_Eknot + impHloc(1,1,iorb,iorb)*Nup(iorb)*gs_weight
                ed_Eknot = ed_Eknot + impHloc(Nspin,Nspin,iorb,iorb)*Ndw(iorb)*gs_weight
             enddo
             ! !> H_imp: Off-diagonal elements, i.e. non-local part. 
             do iorb=1,Norb
                do jorb=1,Norb
                   !SPIN UP
                   Jcondition = &
                        (impHloc(1,1,iorb,jorb)/=zero) .AND. &
                        (ib(jorb)==1)                  .AND. &
                        (ib(iorb)==0)
                   if (Jcondition) then
                      call c(jorb,m,k1,sg1)
                      call cdg(iorb,k1,k2,sg2)
                      j_el=binary_search(sectorI%H(1)%map,k2)
                      j   = j_el + (iph-1)*sectorI%DimEl
                      ed_Eknot = ed_Eknot + impHloc(1,1,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
                   endif
                   !SPIN DW
                   Jcondition = &
                        (impHloc(Nspin,Nspin,iorb,jorb)/=zero) .AND. &
                        (ib(jorb+Ns)==1)                       .AND. &
                        (ib(iorb+Ns)==0)
                   if (Jcondition) then
                      call c(jorb+Ns,m,k1,sg1)
                      call cdg(iorb+Ns,k1,k2,sg2)
                      j_el=binary_search(sectorI%H(1)%map,k2)
                      j   = j_el + (iph-1)*sectorI%DimEl
                      ed_Eknot = ed_Eknot + impHloc(Nspin,Nspin,iorb,jorb)*sg1*sg2*state_cvec(i)*conjg(state_cvec(j))*peso
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
                         j_el=binary_search(sectorI%H(1)%map,k4)
                         j   = j_el + (iph-1)*sectorI%DimEl
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
                         j_el=binary_search(sectorI%H(1)%map,k4)
                         j   = j_el + (iph-1)*sectorI%DimEl
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
    !
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
  end subroutine local_energy_superc



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
    write(unit,"(A)") "# magz_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ",(reg(txtfy(iorb))//"magz_"//reg(txtfy(iorb)),iorb=1,Norb)
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
    write(unit,"(A1,90(A10,6X))") "# ",((reg(txtfy(iorb+(jorb-1)*Norb))//"phisc_"//reg(txtfy(iorb)),iorb=1,Norb),jorb=1,Norb)
    write(unit,"(A)") "# *****"
    write(unit,"(A)") "# imp_last.ed"
    write(unit,"(A1,90(A10,6X))") "# ", "1s2tot", "2egs", "3nph", "4w_ph", "5X_ph", "6X2_ph"
    close(unit)
    
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
    open(unit,file="dens_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") (dens(iorb),iorb=1,Norb)
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
    open(unit,file="magz_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") (magz(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="docc_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") (docc(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Sz2_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") ((sz2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="n2_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") ((n2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Z_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") ((zimp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="sig_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") ((simp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="phisc_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") ((phiscAB(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="imp_all.ed",position='append')
    write(unit,"(90(F15.9,1X))") s2tot, egs, dens_ph, w_ph, X_ph, X2_ph
    close(unit)
    !
    !LAST OBSERVABLES
    unit = free_unit()
    open(unit,file="dens_last.ed")
    write(unit,"(90(F15.9,1X))") (dens(iorb),iorb=1,Norb)
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
    open(unit,file="magz_last.ed")
    write(unit,"(90(F15.9,1X))") (magz(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="docc_last.ed")
    write(unit,"(90(F15.9,1X))") (docc(iorb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Sz2_last.ed")
    write(unit,"(90(F15.9,1X))") ((sz2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="n2_last.ed")
    write(unit,"(90(F15.9,1X))") ((n2(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="Z_last.ed")
    write(unit,"(90(F15.9,1X))") ((zimp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="sig_last.ed")
    write(unit,"(90(F15.9,1X))") ((simp(iorb,ispin),iorb=1,Norb),ispin=1,Nspin)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="phisc_last.ed")
    write(unit,"(90(F15.9,1X))") ((phiscAB(iorb,jorb),jorb=1,Norb),iorb=1,Norb)
    close(unit)
    !
    unit = free_unit()
    open(unit,file="imp_last.ed")
    write(unit,"(90(F15.9,1X))") s2tot, egs, dens_ph, w_ph, X_ph, X2_ph
    close(unit)
    !
    !PARAMETERS
    unit = free_unit()
    open(unit,file="parameters_last"//reg(ed_file_suffix)//".ed")
    write(unit,"(90F15.9)")xmu,beta,(uloc(iorb),iorb=1,Norb),Ust,Jh,Jx,Jp
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
    write(unit,"(90F15.9)")ed_Epot,ed_Epot-ed_Ehartree,ed_Eknot,ed_Ehartree,ed_Dust,ed_Dund,ed_Dse,ed_Dph
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
    complex(8),dimension(:) :: vec
    real(8)              :: psi(0:DimPh-1)
    real(8)              :: x,dx
    integer              :: i,j,j_ph,val
    integer              :: istart,jstart,iend,jend
    !
    dx = (xmax-xmin)/dble(Lpos)
    !
    x = xmin
    do i=1,Lpos !cycle over x
       call Hermite(x,psi)
       !
       istart = i_el + (iph-1)*sectorI%DimEl !subroutine already inside a sectorI cycle
       !
       !phonon diagonal part
       pdf_ph(i) = pdf_ph(i) + peso*psi(iph-1)*psi(iph-1)*abs(vec(istart))**2
       pdf_part(i,val) = pdf_part(i,val) + peso*psi(iph-1)*psi(iph-1)*abs(vec(istart))**2
       !
       !phonon off-diagonal part
       do j_ph=iph+1,DimPh
          jstart = i_el + (j_ph-1)*sectorI%DimEl
          pdf_ph(i)       = pdf_ph(i)       + peso*psi(iph-1)*psi(j_ph-1)*2.d0*real( vec(istart)*conjg(vec(jstart)) )
          pdf_part(i,val) = pdf_part(i,val) + peso*psi(iph-1)*psi(j_ph-1)*2.d0*real( vec(istart)*conjg(vec(jstart)) )
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



end MODULE ED_OBSERVABLES_SUPERC
