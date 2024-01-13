subroutine ed_get_g0imp_site_n3(self,bath,axis,type)
  complex(8),dimension(:,:,:),intent(inout)   :: self
  real(8),dimension(:)                        :: bath
  character(len=*),optional                   :: axis
  character(len=*),optional                   :: type
  character(len=1)                            :: axis_
  character(len=1)                            :: type_
  complex(8),dimension(:,:,:,:,:),allocatable :: g0
  complex(8),dimension(:),allocatable         :: x
  integer                                     :: L
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  call allocate_grids
  !  
  select case (axis_)
  case default;stop "ed_get_g0imp ERROR: axis is neither Matsubara, nor Realaxis"
  case ('m','M')
     L = Lmats
     allocate(x(L))
     x=xi*wm
  case('r','R')
     L = Lreal
     allocate(x(L))
     x=dcmplx(wr,eps)
  end select
  !
  allocate(g0(Nspin,Nspin,Norb,Norb,L))
  call ed_get_g0and(x,bath,g0,axis=axis_,type=type_)
  !
  call assert_shape(self,[Nspin*Norb,Nspin*Norb,L],'ed_get_g0imp','self')
  self = nn2so_reshape(g0,Nspin,Norb,L)
end subroutine ed_get_g0imp_site_n3

subroutine ed_get_g0imp_site_n5(self,bath,axis,type)
  complex(8),dimension(:,:,:,:,:),intent(inout) :: self
  real(8),dimension(:)                          :: bath
  character(len=*),optional                     :: axis
  character(len=*),optional                     :: type
  character(len=1)                              :: axis_
  character(len=1)                              :: type_
  complex(8),dimension(:,:,:,:,:),allocatable   :: g0
  complex(8),dimension(:),allocatable           :: x
  integer                                       :: L
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  call allocate_grids
  !  
  select case (axis_)
  case default;stop "ed_get_g0imp ERROR: axis is neither Matsubara, nor Realaxis"
  case ('m','M')
     L = Lmats
     allocate(x(L))
     x=xi*wm
  case('r','R')
     L = Lreal
     allocate(x(L))
     x=dcmplx(wr,eps)
  end select
  !
  allocate(g0(Nspin,Nspin,Norb,Norb,L))
  call ed_get_g0and(x,bath,g0,axis=axis_,type=type_)
  !
  call assert_shape(self,[Nspin,Nspin,Norb,Norb,L],'ed_get_g0imp','self')
  self = g0
end subroutine ed_get_g0imp_site_n5





!##################################################################
!##################################################################
!##################################################################

subroutine ed_get_g0imp_lattice_n3(self,bath,axis,type)
  complex(8),dimension(:,:,:),intent(inout)     :: self
  real(8),dimension(:,:)                        :: bath
  character(len=*),optional                     :: axis
  character(len=*),optional                     :: type
  character(len=1)                              :: axis_
  character(len=1)                              :: type_
  complex(8),dimension(:,:,:,:,:,:),allocatable :: g0
  complex(8),dimension(:),allocatable           :: x
  integer                                       :: L,Nsites,isite
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  call allocate_grids
  select case (axis_)
  case default;stop "ed_get_g0imp ERROR: axis is neither Matsubara, nor Realaxis"
  case ('m','M')
     L = Lmats
     allocate(x(L))
     x=xi*wm
  case('r','R')
     L = Lreal
     allocate(x(L))
     x=dcmplx(wr,eps)
  end select
  !
  Nsites=size(bath,1)
  allocate(g0(Nsites,Nspin,Nspin,Norb,Norb,L))
  do isite=1,Nsites
     call ed_get_g0and(x,bath(isite,:),g0(isite,:,:,:,:,:),axis=axis_,type=type_)
  enddo
  !
  call assert_shape(self,[Nsites*Nspin*Norb,Nsites*Nspin*Norb,L],'ed_get_g0imp','self')
  self = nnn2lso_reshape(g0,Nsites,Nspin,Norb,L)
end subroutine ed_get_g0imp_lattice_n3

subroutine ed_get_g0imp_lattice_n4(self,bath,axis,type)
  complex(8),dimension(:,:,:,:),intent(inout)        :: self
  real(8),dimension(:,:)                        :: bath
  character(len=*),optional                     :: axis
  character(len=*),optional                     :: type
  character(len=1)                              :: axis_
  character(len=1)                              :: type_
  complex(8),dimension(:,:,:,:,:,:),allocatable :: g0
  complex(8),dimension(:),allocatable           :: x
  integer                                       :: L,Nsites,isite
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  call allocate_grids
  select case (axis_)
  case default;stop "ed_get_g0imp ERROR: axis is neither Matsubara, nor Realaxis"
  case ('m','M')
     L = Lmats
     allocate(x(L))
     x=xi*wm
  case('r','R')
     L = Lreal
     allocate(x(L))
     x=dcmplx(wr,eps)
  end select
  !
  Nsites=size(bath,1)
  allocate(g0(Nsites,Nspin,Nspin,Norb,Norb,L))
  do isite=1,Nsites
     call ed_get_g0and(x,bath(isite,:),g0(isite,:,:,:,:,:),axis=axis_,type=type_)
  enddo
  !
  call assert_shape(self,[Nsites,Nspin*Norb,Nspin*Norb,L],'ed_get_g0imp','self')
  do isite=1,Nsites
     self(isite,:,:,:) = nn2so_reshape(g0(isite,:,:,:,:,:),Nspin,Norb,L)
  enddo
end subroutine ed_get_g0imp_lattice_n4


subroutine ed_get_g0imp_lattice_n6(self,bath,axis,type)
  complex(8),dimension(:,:,:,:,:,:),intent(inout)        :: self
  real(8),dimension(:,:)                        :: bath
  character(len=*),optional                     :: axis
  character(len=*),optional                     :: type
  character(len=1)                              :: axis_
  character(len=1)                              :: type_
  complex(8),dimension(:,:,:,:,:,:),allocatable :: g0
  complex(8),dimension(:),allocatable           :: x
  integer                                       :: L,Nsites,isite
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  call allocate_grids
  select case (axis_)
  case default;stop "ed_get_g0imp ERROR: axis is neither Matsubara, nor Realaxis"
  case ('m','M')
     L = Lmats
     allocate(x(L))
     x=xi*wm
  case('r','R')
     L = Lreal
     allocate(x(L))
     x=dcmplx(wr,eps)
  end select
  !
  Nsites=size(bath,1)
  allocate(g0(Nsites,Nspin,Nspin,Norb,Norb,L))
  do isite=1,Nsites
     call ed_get_g0and(x,bath(isite,:),g0(isite,:,:,:,:,:),axis=axis_,type=type_)
  enddo
  !
  call assert_shape(self,[Nsites,Nspin,Nspin,Norb,Norb,L],'ed_get_g0imp','self')
  self = g0
end subroutine ed_get_g0imp_lattice_n6
