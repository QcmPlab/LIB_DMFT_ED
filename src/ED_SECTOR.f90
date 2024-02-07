MODULE ED_SECTOR
  USE ED_INPUT_VARS
  USE ED_VARS_GLOBAL
  USE ED_AUX_FUNX
  USE SF_TIMER
  USE SF_IOTOOLS, only:free_unit,reg,file_length
#ifdef _MPI
  USE MPI
  USE SF_MPI
#endif
  implicit none
  private

  interface map_allocate
     module procedure :: map_allocate_scalar
     module procedure :: map_allocate_vector
  end interface map_allocate

  interface map_deallocate
     module procedure :: map_deallocate_scalar
     module procedure :: map_deallocate_vector
  end interface map_deallocate

  interface flip_state
     module procedure :: flip_state_normal
     module procedure :: flip_state_other
  end interface flip_state

  interface get_Sector
     module procedure :: get_Sector_normal
     module procedure :: get_Sector_superc
     module procedure :: get_Sector_nonsu2
  end interface get_Sector

  interface get_QuantumNumbers
     module procedure :: get_QuantumNumbers_normal
     module procedure :: get_QuantumNumbers_other
  end interface get_QuantumNumbers


  public :: build_sector
  public :: delete_sector
  !
  public :: apply_op_C
  public :: apply_op_CDG
  public :: apply_op_Sz
  public :: apply_op_N
  public :: build_op_Ns
  !
  public :: get_Sector
  public :: get_QuantumNumbers
  public :: get_Nup
  public :: get_Ndw
  public :: get_Sz
  public :: get_Ntot
  public :: get_DimUp
  public :: get_DimDw
  public :: get_Dim
  !
  public :: indices2state
  public :: state2indices
  public :: iup_index
  public :: idw_index


  public :: twin_sector_order
  public :: get_twin_sector
  public :: flip_state






contains




  !##################################################################
  !##################################################################
  !BUILD SECTORS
  !##################################################################
  !##################################################################
  subroutine build_sector(isector,self)
    integer,intent(in) :: isector
    type(sector)       :: self
    integer            :: iup,idw
    integer            :: nup_,ndw_,sz_,nt_
    integer            :: twoSz_,twoLz_,twoJz
    integer            :: dim,iud,i,ibath,iorb
    integer            :: ivec(Ns),jvec(Ns)
    !
    !
    if(self%status)call delete_sector(self)
    !
    self%index = isector
    !
    select case(ed_mode)
    case default
       allocate(self%H(2*Ns_Ud))
       allocate(self%DimUps(Ns_Ud))
       allocate(self%DimDws(Ns_Ud))
       allocate(self%Nups(Ns_Ud))
       allocate(self%Ndws(Ns_Ud))
       !
       call get_Nup(isector,self%Nups);self%Nup=sum(self%Nups)
       call get_Ndw(isector,self%Ndws);self%Ndw=sum(self%Ndws)
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A,2"//str(Ns_Ud)//"I3)")&
            "DEBUG build_sector: sector:"//str(isector)//"- Nups,Ndws",self%Nups,self%Ndws
#endif
       call get_DimUp(isector,self%DimUps);self%DimUp=product(self%DimUps)
       call get_DimDw(isector,self%DimDws);self%DimDw=product(self%DimDws)
       self%DimEl=self%DimUp*self%DimDw
       self%DimPh=Nph+1
       self%Dim=self%DimEl*self%DimPh
       !
       call map_allocate(self%H,[self%DimUps,self%DimDws])
       do iud=1,Ns_Ud
          !UP    
          dim=0
          do iup=0,2**Ns_Orb-1
             nup_ = popcnt(iup)
             if(nup_ /= self%Nups(iud))cycle
             dim  = dim+1
             self%H(iud)%map(dim) = iup
          enddo
          !DW
          dim=0
          do idw=0,2**Ns_Orb-1
             ndw_= popcnt(idw)
             if(ndw_ /= self%Ndws(iud))cycle
             dim = dim+1
             self%H(iud+Ns_Ud)%map(dim) = idw
          enddo
       enddo
       !
    case ("superc")
       allocate(self%H(1))
       self%Sz    = getSz(isector)
#ifdef _DEBUG
       if(ed_verbose>3)write(Logfile,"(A,I4)")&
            "DEBUG build_sector: sector:"//str(isector)//"- Sz",self%Sz
#endif
       self%DimPh = Nph+1
       self%DimEl = getDim(isector)/(Nph+1)
       self%Dim   = self%DimEl*self%DimPh
       call map_allocate(self%H,[self%DimEl])
       dim=0
       do idw=0,2**Ns-1
          ndw_= popcnt(idw)
          do iup=0,2**Ns-1
             nup_ = popcnt(iup)
             sz_  = nup_ - ndw_
             if(sz_ == self%Sz)then
                dim=dim+1
                self%H(1)%map(dim) = iup + idw*2**Ns
             endif
          enddo
       enddo

    case ("nonsu2")
       allocate(self%H(1))
       if(Jz_basis)then
          self%Ntot  = getN(isector)
#ifdef _DEBUG
          if(ed_verbose>3)write(Logfile,"(A,I4)")&
               "DEBUG build_sector: sector:"//str(isector)//"- N",self%Ntot
#endif
          self%DimEl = getDim(isector)/(Nph+1)
          self%DimPh = Nph+1
          self%Dim   = self%DimEl*self%DimPh
          self%twoJz = gettwoJz(isector)
          call map_allocate(self%H,[self%Dim])
          dim=0
          do idw=0,2**Ns-1
             ndw_= popcnt(idw)
             jvec = bdecomp(idw,Ns)
             do iup=0,2**Ns-1
                nup_ = popcnt(iup)
                ivec = bdecomp(iup,Ns)
                nt_     = nup_ + ndw_
                twoSz_  = nup_ - ndw_
                twoLz_  = 0
                do ibath=0,Nbath
                   do iorb=1,Norb
                      twoLz_ = twoLz_ + 2 * Lzdiag(iorb) * ivec(iorb+Norb*ibath)  &
                           + 2 * Lzdiag(iorb) * jvec(iorb+Norb*ibath)
                   enddo
                enddo
                if(self%Ntot == nt_  .AND. self%twoJz==(twoSz_+twoLz_) )then
                   dim=dim+1
                   self%H(1)%map(dim) = iup + idw*2**Ns
                endif
             enddo
          enddo
       else
          self%Ntot  = getN(isector)
#ifdef _DEBUG
          if(ed_verbose>4)write(Logfile,"(A,I4)")&
               "DEBUG build_sector: sector:"//str(isector)//"- N",self%Ntot
#endif
          self%DimEl = getDim(isector)/(Nph+1)
          self%DimPh = Nph+1
          self%Dim   = self%DimEl*self%DimPh
          call map_allocate(self%H,[self%Dim])
          dim=0
          do idw=0,2**Ns-1
             ndw_= popcnt(idw)
             do iup=0,2**Ns-1
                nup_ = popcnt(iup)
                nt_  = nup_ + ndw_
                if(nt_ == self%Ntot)then
                   dim=dim+1
                   self%H(1)%map(dim) = iup + idw*2**Ns
                endif
             enddo
          enddo
       endif
    end select
    self%Nlanc = min(self%Dim,lanc_nGFiter)
    self%status=.true.
  end subroutine build_sector


  subroutine delete_sector(self)
    type(sector) :: self
#ifdef _DEBUG
    if(ed_verbose>4)write(Logfile,"(A,I4)")"DEBUG delete_sector"
#endif
    call map_deallocate(self%H)
    if(allocated(self%H))deallocate(self%H)
    if(allocated(self%DimUps))deallocate(self%DimUps)
    if(allocated(self%DimDws))deallocate(self%DimDws)
    if(allocated(self%Nups))deallocate(self%Nups)
    if(allocated(self%Ndws))deallocate(self%Ndws)
    self%index=0
    self%DimUp=0
    self%DimDw=0
    self%Dim=0
    self%Nup=0
    self%Ndw=0
    self%Sz=-1000
    self%Ntot=-1
    self%twoJz=-1000
    self%Nlanc=0
    self%status=.false.
  end subroutine delete_sector







  subroutine map_allocate_scalar(H,N)
    type(sector_map) :: H
    integer          :: N
    if(H%status) call map_deallocate_scalar(H)
    allocate(H%map(N))
    H%status=.true.
  end subroutine map_allocate_scalar
  !
  subroutine map_allocate_vector(H,N)
    type(sector_map),dimension(:)       :: H
    integer,dimension(size(H))          :: N
    integer                             :: i
    do i=1,size(H)
       call map_allocate_scalar(H(i),N(i))
    enddo
  end subroutine map_allocate_vector



  subroutine map_deallocate_scalar(H)
    type(sector_map) :: H
    if(.not.H%status)then
       write(*,*) "WARNING map_deallocate_scalar: H is not allocated"
       return
    endif
    if(allocated(H%map))deallocate(H%map)
    H%status=.false.
  end subroutine map_deallocate_scalar
  !
  subroutine map_deallocate_vector(H)
    type(sector_map),dimension(:) :: H
    integer                       :: i
    do i=1,size(H)
       call map_deallocate_scalar(H(i))
    enddo
  end subroutine map_deallocate_vector








  !> i=instate , j=outstate, ipos=site+orb index, ialfa=?, ispin, sectorI=sector in,sectorJ=sector out
  subroutine apply_op_C(i,j,sgn,ipos,ialfa,ispin,sectorI,sectorJ) 
    integer, intent(in)         :: i,ipos,ialfa,ispin
    type(sector),intent(in)     :: sectorI,sectorJ
    integer,intent(out)         :: j
    real(8),intent(out)         :: sgn
    integer                     :: ibeta,isite
    integer                     :: r
    integer                     :: iph,i_el,j_el,el_state
    integer,dimension(2*Ns_Ud)  :: Indices
    integer,dimension(2*Ns_Ud)  :: Jndices
    integer,dimension(2,Ns_Orb) :: Nud !Nbits(Ns_Orb)
    integer,dimension(2)        :: Iud
    integer,dimension(2*Ns)     :: ib
    !
    j=0
    sgn=0d0
    !
    select case(ed_mode)
    case default
       ibeta  = ialfa + (ispin-1)*Ns_Ud
       iph = (i-1)/(sectorI%DimEl) + 1
       i_el = mod(i-1,sectorI%DimEl) + 1
       !
       call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
       iud(1)   = sectorI%H(ialfa)%map(Indices(ialfa))
       iud(2)   = sectorI%H(ialfa+Ns_Ud)%map(Indices(ialfa+Ns_Ud))
       nud(1,:) = Bdecomp(iud(1),Ns_Orb)
       nud(2,:) = Bdecomp(iud(2),Ns_Orb)
       if(Nud(ispin,ipos)/=1)return
       call c(ipos,iud(ispin),r,sgn)
       Jndices        = Indices
       Jndices(ibeta) = binary_search(sectorJ%H(ibeta)%map,r)
       call indices2state(Jndices,[sectorJ%DimUps,sectorJ%DimDws],j)
       !
       j = j + (iph-1)*sectorJ%DimEl
       !
    case("superc","nonsu2")
       isite= ipos + (ispin-1)*Ns
       iph  = (i-1)/(sectorI%DimEl)+1
       i_el = mod(i-1,sectorI%DimEl)+1
       el_state = sectorI%H(1)%map(i_el)
       ib   = bdecomp(el_state,2*Ns)
       if(ib(isite)/=1)return
       call c(isite,el_state,r,sgn)
       !
       j_el    = binary_search(sectorJ%H(1)%map,r)
       j = j_el + (iph-1)*sectorJ%DimEl
       !
    end select
  end subroutine apply_op_C


  subroutine apply_op_CDG(i,j,sgn,ipos,ialfa,ispin,sectorI,sectorJ) 
    integer, intent(in)         :: i,ipos,ialfa,ispin
    type(sector),intent(in)     :: sectorI,sectorJ
    integer,intent(out)         :: j
    real(8),intent(out)         :: sgn
    integer                     :: ibeta,isite
    integer                     :: r
    integer                     :: iph,i_el,j_el,el_state
    integer,dimension(2*Ns_Ud)  :: Indices
    integer,dimension(2*Ns_Ud)  :: Jndices
    integer,dimension(2,Ns_Orb) :: Nud !Nbits(Ns_Orb)
    integer,dimension(2)        :: Iud
    integer,dimension(2*Ns)     :: ib
    !
    j=0
    sgn=0d0
    !
    select case(ed_mode)
    case default
       ibeta  = ialfa + (ispin-1)*Ns_Ud
       iph = (i-1)/(sectorI%DimEl) + 1
       i_el = mod(i-1,sectorI%DimEl) + 1
       !
       call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
       iud(1)   = sectorI%H(ialfa)%map(Indices(ialfa))
       iud(2)   = sectorI%H(ialfa+Ns_Ud)%map(Indices(ialfa+Ns_Ud))
       nud(1,:) = Bdecomp(iud(1),Ns_Orb)
       nud(2,:) = Bdecomp(iud(2),Ns_Orb)
       if(Nud(ispin,ipos)/=0)return
       call cdg(ipos,iud(ispin),r,sgn)
       Jndices        = Indices
       Jndices(ibeta) = binary_search(sectorJ%H(ibeta)%map,r)
       call indices2state(Jndices,[sectorJ%DimUps,sectorJ%DimDws],j_el)
       !
       j = j_el + (iph-1)*sectorJ%DimEl
       !
    case("superc","nonsu2")
       isite= ipos + (ispin-1)*Ns
       iph  = (i-1)/(sectorI%DimEl)+1
       i_el = mod(i-1,sectorI%DimEl) + 1
       el_state = sectorI%H(1)%map(i_el)
       ib   = bdecomp(el_state,2*Ns)
       if(ib(isite)/=0)return
       call cdg(isite,el_state,r,sgn)
       !
       j_el    = binary_search(sectorJ%H(1)%map,r)
       j = j_el + (iph-1)*sectorJ%DimEl
       !
    end select
  end subroutine apply_op_CDG


  subroutine apply_op_Sz(i,sgn,ipos,ialfa,sectorI) 
    integer, intent(in)         :: i,ipos,ialfa
    type(sector),intent(in)     :: sectorI
    real(8),intent(out)         :: sgn
    integer                     :: iph,i_el
    integer,dimension(2*Ns_Ud)  :: Indices
    integer,dimension(2*Ns_Ud)  :: Jndices
    integer,dimension(2,Ns_Orb) :: Nud !Nbits(Ns_Orb)
    integer,dimension(2)        :: Iud
    !
    sgn=0d0
    !
    iph = (i-1)/(sectorI%DimEl) + 1
    i_el = mod(i-1,sectorI%DimEl) + 1
    !
    call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
    iud(1)   = sectorI%H(ialfa)%map(Indices(ialfa))
    iud(2)   = sectorI%H(ialfa+Ns_Ud)%map(Indices(ialfa+Ns_Ud))
    nud(1,:) = Bdecomp(iud(1),Ns_Orb)
    nud(2,:) = Bdecomp(iud(2),Ns_Orb)
    !
    sgn = dble(nud(1,ipos))-dble(nud(2,ipos))
    sgn = sgn/2d0
  end subroutine apply_op_Sz


  subroutine apply_op_N(i,sgn,ipos,ialfa,sectorI) 
    integer, intent(in)         :: i,ipos,ialfa
    type(sector),intent(in)     :: sectorI
    real(8),intent(out)         :: sgn
    integer                     :: iph,i_el
    integer,dimension(2*Ns_Ud)  :: Indices
    integer,dimension(2*Ns_Ud)  :: Jndices
    integer,dimension(2,Ns_Orb) :: Nud !Nbits(Ns_Orb)
    integer,dimension(2)        :: Iud
    !
    sgn=0d0
    !
    iph = (i-1)/(sectorI%DimEl) + 1
    i_el = mod(i-1,sectorI%DimEl) + 1
    !
    call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
    iud(1)   = sectorI%H(ialfa)%map(Indices(ialfa))
    iud(2)   = sectorI%H(ialfa+Ns_Ud)%map(Indices(ialfa+Ns_Ud))
    nud(1,:) = Bdecomp(iud(1),Ns_Orb)
    nud(2,:) = Bdecomp(iud(2),Ns_Orb)
    !
    sgn = dble(nud(1,ipos))+dble(nud(2,ipos))
  end subroutine apply_op_N


  subroutine build_op_Ns(i,Nup,Ndw,sectorI) 
    integer, intent(in)             :: i
    type(sector),intent(in)         :: sectorI
    integer,dimension(Ns)           :: Nup,Ndw  ![Ns]
    integer                         :: iph,i_el,ii,iorb
    integer,dimension(2*Ns_Ud)      :: Indices
    integer,dimension(Ns_Ud,Ns_Orb) :: Nups,Ndws  ![1,Ns]-[Norb,1+Nbath]
    integer,dimension(2*Ns)         :: Ib
    integer,dimension(2)            :: Iud
    !
    select case(ed_mode)
    case default
       iph = (i-1)/(sectorI%DimEl) + 1
       i_el = mod(i-1,sectorI%DimEl) + 1
       !
       call state2indices(i_el,[sectorI%DimUps,sectorI%DimDws],Indices)
       do ii=1,Ns_Ud
          iud(1) = sectorI%H(ii)%map(Indices(ii))
          iud(2) = sectorI%H(ii+Ns_Ud)%map(Indices(ii+Ns_ud))
          Nups(ii,:) = Bdecomp(iud(1),Ns_Orb) ![Norb,1+Nbath]
          Ndws(ii,:) = Bdecomp(iud(2),Ns_Orb)
       enddo
       Nup = Breorder(Nups)
       Ndw = Breorder(Ndws)
       !
    case("superc","nonsu2")
       ii = sectorI%H(1)%map(i)
       Ib = bdecomp(ii,2*Ns)
       Nup = Ib(1:Ns)
       Ndw = Ib(Ns+1:)
       ! do ii=1,Ns
       !    Nup(ii)= ib(ii)
       !    Ndw(ii)= ib(ii+Ns)
       ! enddo
    end select
    !
  end subroutine build_op_Ns







  subroutine get_Sector_normal(QN,N,isector)
    integer,dimension(:) :: QN
    integer              :: N
    integer              :: isector
    integer              :: i,Nind,factor
    Nind = size(QN)
    Factor = N+1
    isector = 1
    do i=Nind,1,-1
       isector = isector + QN(i)*(Factor)**(Nind-i)
    enddo
  end subroutine get_Sector_normal
  !
  subroutine get_Sector_superc(QN,isector)
    integer :: QN
    integer :: isector
    isector=getSector(QN,1)
  end subroutine get_Sector_superc
  !
  subroutine get_Sector_nonsu2(QN,twoJz,isector)
    integer :: QN,twoJz
    integer :: isector
    if(Jz_basis)then
       isector=getSector(QN,twoJz)
    else
       isector=getSector(Qn,1)
    end if
  end subroutine get_Sector_nonsu2


  subroutine get_QuantumNumbers_normal(isector,N,QN)
    integer                          :: isector,N
    integer,dimension(:)             :: QN
    integer                          :: i,count,Dim
    integer,dimension(size(QN)) :: QN_
    !
    Dim = size(QN)
    if(mod(Dim,2)/=0)stop "get_QuantumNumbers error: Dim%2 != 0"
    count=isector-1
    do i=1,Dim
       QN_(i) = mod(count,N+1)
       count      = count/(N+1)
    enddo
    QN = QN_(Dim:1:-1)
  end subroutine get_QuantumNumbers_normal
  subroutine get_QuantumNumbers_other(isector,QN)
    integer                     :: isector
    integer                     :: QN
    select case(ed_mode)
    case ("superc")
       QN=getSz(isector)
    case("nonsu2")
       QN=getN(isector)
    case default
       stop "get_QuantumNumbers_other ERROR: invoked with ed_mode=normal"
    end select
  end subroutine get_QuantumNumbers_other


  subroutine get_Nup(isector,Nup)
    integer                   :: isector,Nup(Ns_Ud)
    integer                   :: i,count
    integer,dimension(2*Ns_Ud)  :: indices_
    count=isector-1
    do i=1,2*Ns_Ud
       indices_(i) = mod(count,Ns_Orb+1)
       count      = count/(Ns_Orb+1)
    enddo
    Nup = indices_(2*Ns_Ud:Ns_Ud+1:-1)
  end subroutine get_Nup
  !
  subroutine get_Ndw(isector,Ndw)
    integer                   :: isector,Ndw(Ns_Ud)
    integer                   :: i,count
    integer,dimension(2*Ns_Ud) :: indices_
    count=isector-1
    do i=1,2*Ns_Ud
       indices_(i) = mod(count,Ns_Orb+1)
       count      = count/(Ns_Orb+1)
    enddo
    Ndw = indices_(Ns_Ud:1:-1)
  end subroutine get_Ndw

  subroutine get_Sz(isector,Sz)
    integer :: isector,Sz
    Sz = getSz(isector)
  end subroutine get_Sz

  subroutine get_Ntot(isector,Ntot)
    integer :: isector,Ntot
    Ntot = getN(isector)
  end subroutine get_Ntot



  subroutine  get_DimUp(isector,DimUps)
    integer                :: isector,DimUps(Ns_Ud)
    integer                :: Nups(Ns_Ud),iud
    call get_Nup(isector,Nups)
    do iud=1,Ns_Ud
       DimUps(iud) = binomial(Ns_Orb,Nups(iud))
    enddo
  end subroutine get_DimUp

  subroutine get_DimDw(isector,DimDws)
    integer                :: isector,DimDws(Ns_Ud)
    integer                :: Ndws(Ns_Ud),iud
    call get_Ndw(isector,Ndws)
    do iud=1,Ns_Ud
       DimDws(iud) = binomial(Ns_Orb,Ndws(iud))
    enddo
  end subroutine get_DimDw

  subroutine  get_Dim(isector,Dim)
    integer                :: isector,Dim
    Dim=getDim(isector)
  end subroutine get_Dim


  subroutine indices2state(ivec,Nvec,istate)
    integer,dimension(:)          :: ivec
    integer,dimension(size(ivec)) :: Nvec
    integer                       :: istate,i
    istate=ivec(1)
    do i=2,size(ivec)
       istate = istate + (ivec(i)-1)*product(Nvec(1:i-1))
    enddo
  end subroutine indices2state

  subroutine state2indices(istate,Nvec,ivec)
    integer                       :: istate
    integer,dimension(:)          :: Nvec
    integer,dimension(size(Nvec)) :: Ivec
    integer                       :: i,count,N
    count = istate-1
    N     = size(Nvec)
    do i=1,N
       Ivec(i) = mod(count,Nvec(i))+1
       count   = count/Nvec(i)
    enddo
  end subroutine state2indices


  function iup_index(i,DimUp) result(iup)
    integer :: i
    integer :: DimUp
    integer :: iup
    iup = mod(i,DimUp);if(iup==0)iup=DimUp
  end function iup_index


  function idw_index(i,DimUp) result(idw)
    integer :: i
    integer :: DimUp
    integer :: idw
    idw = (i-1)/DimUp+1
  end function idw_index














  !##################################################################
  !##################################################################
  !TWIN SECTORS ROUTINES:
  !##################################################################
  !##################################################################

  !+------------------------------------------------------------------+
  !PURPOSE  : Build the re-ordering map to go from sector A(nup,ndw)
  ! to its twin sector B(ndw,nup), with nup!=ndw.
  !
  !- build the map from the A-sector to \HHH
  !- get the list of states in \HHH corresponding to sector B twin of A
  !- return the ordering of B-states in \HHH with respect to those of A
  !+------------------------------------------------------------------+
  subroutine twin_sector_order(isector,order)
    integer                             :: isector
    integer,dimension(:)                :: order
    type(sector)                        :: sectorH
    type(sector_map),dimension(2*Ns_Ud) :: H
    integer,dimension(2*Ns_Ud)          :: Indices,Istates
    integer                             :: i,iud,iph,i_el
    !
    if(size(Order)/=GetDim(isector))stop "twin_sector_order error: wrong dimensions of *order* array"
    !
    call build_sector(isector,sectorH)
    select case(ed_mode)
    case ("normal")
       do i=1,sectorH%Dim
          iph = (i-1)/(sectorH%DimEl) + 1   !find number of phonons
          i_el = mod(i-1,sectorH%DimEl) + 1 !electronic index
          call state2indices(i_el,[sectorH%DimUps,sectorH%DimDws],Indices)
          forall(iud=1:2*Ns_Ud)Istates(iud) = sectorH%H(iud)%map(Indices(iud))        
          Order(i) = flip_state( Istates ) + (iph-1)*2**(2*Ns)          
       enddo
    case default
       do i=1,sectorH%dim
          iph = (i-1)/(sectorH%DimEl) + 1   !find number of phonons
          i_el = mod(i-1,sectorH%DimEl) + 1 !electronic index
          Order(i) = flip_state(sectorH%H(1)%map(i_el)) + (iph-1)*2**(2*Ns)     
       enddo
    end select
    call delete_sector(sectorH)
    call sort_array(Order) 
  end subroutine twin_sector_order



  !+------------------------------------------------------------------+
  !PURPOSE  : Flip an Hilbert space state m=|{up}>|{dw}> into:
  !
  ! normal: j=|{dw}>|{up}>  , nup --> ndw
  ! superc: j=|{dw}>|{up}>  , sz  --> -sz
  ! nonsu2: j=|{!up}>|{!dw}>, n   --> 2*Ns-n
  !+------------------------------------------------------------------+
  function flip_state_normal(istate) result(j)
    integer,dimension(2*Ns_Ud) :: istate
    integer                    :: j
    integer,dimension(Ns_Ud)   :: jups,jdws
    integer,dimension(2*Ns_Ud) :: dims
    jups = istate(Ns_Ud+1:2*Ns_Ud)
    jdws = istate(1:Ns_Ud)
    dims = 2**Ns_Orb
    call indices2state([jups,jdws],Dims,j)
  end function flip_state_normal
  function flip_state_other(istate) result(j)
    integer          :: istate
    integer          :: j
    integer          :: ivec(2*Ns),foo(2*Ns)
    !
    Ivec = bdecomp(istate,2*Ns)
    select case(ed_mode)
    case("superc")  !Invert the overall spin sign: |{up}> <---> |{dw}>
       foo(1:Ns)     = Ivec(Ns+1:2*Ns)
       foo(Ns+1:2*Ns)= Ivec(1:Ns)
    case ("nonsu2") !Exchange Occupied sites (1) with Empty sites (0)
       where(Ivec==1)foo=0
       where(Ivec==0)foo=1
    case default    !Exchange UP-config |{up}> with DW-config |{dw}>
       stop "flip_state_other error: called with ed_mode==normal"
    end select
    !
    j = bjoin(foo,2*Ns)
    !
  end function flip_state_other


  !+------------------------------------------------------------------+
  !PURPOSE  : get the twin of a given sector (the one with opposite 
  ! quantum numbers): 
  ! nup,ndw ==> ndw,nup (spin-exchange)
  !+------------------------------------------------------------------+
  function get_twin_sector(isector) result(jsector)
    integer,intent(in)       :: isector
    integer                  :: jsector    
    integer,dimension(Ns_Ud) :: Iups,Idws
    integer                  :: Sz,Ntot
    select case(ed_mode)
    case default
       call get_Nup(isector,iups)
       call get_Ndw(isector,idws)
       call get_Sector([idws,iups],Ns_Orb,jsector)
    case ("superc")
       call get_Sz(isector,Sz)
       Sz = -Sz
       call get_Sector(Sz,jsector)
    case ("nonsu2")
       call get_Ntot(isector,Ntot)
       Ntot = Nlevels-Ntot
       call get_Sector(Ntot,jsector)
    end select
  end function get_twin_sector













  !##################################################################
  !##################################################################
  !AUXILIARY COMPUTATIONAL ROUTINES ARE HERE BELOW:
  !##################################################################
  !##################################################################

  !+------------------------------------------------------------------+
  !PURPOSE : sort array of integer using random algorithm
  !+------------------------------------------------------------------+
  subroutine sort_array(array)
    integer,dimension(:),intent(inout)      :: array
    integer,dimension(size(array))          :: order
    integer                                 :: i
    forall(i=1:size(array))order(i)=i
    call qsort_sort( array, order, 1, size(array) )
    array=order
  contains
    recursive subroutine qsort_sort( array, order, left, right )
      integer, dimension(:)                 :: array
      integer, dimension(:)                 :: order
      integer                               :: left
      integer                               :: right
      integer                               :: i
      integer                               :: last
      if ( left .ge. right ) return
      call qsort_swap( order, left, qsort_rand(left,right) )
      last = left
      do i = left+1, right
         if ( compare(array(order(i)), array(order(left)) ) .lt. 0 ) then
            last = last + 1
            call qsort_swap( order, last, i )
         endif
      enddo
      call qsort_swap( order, left, last )
      call qsort_sort( array, order, left, last-1 )
      call qsort_sort( array, order, last+1, right )
    end subroutine qsort_sort
    !---------------------------------------------!
    subroutine qsort_swap( order, first, second )
      integer, dimension(:)                 :: order
      integer                               :: first, second
      integer                               :: tmp
      tmp           = order(first)
      order(first)  = order(second)
      order(second) = tmp
    end subroutine qsort_swap
    !---------------------------------------------!
    function qsort_rand( lower, upper )
      implicit none
      integer                               :: lower, upper
      real(8)                               :: r
      integer                               :: qsort_rand
      call random_number(r)
      qsort_rand =  lower + nint(r * (upper-lower))
    end function qsort_rand
    function compare(f,g)
      integer                               :: f,g
      integer                               :: compare
      compare=1
      if(f<g)compare=-1
    end function compare
  end subroutine sort_array



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



end MODULE ED_SECTOR












