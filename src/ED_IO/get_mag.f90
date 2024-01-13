subroutine ed_get_mag_n0(self,component,iorb)
  real(8) :: self
  character(len=1),optional :: component
  integer,optional          :: iorb
  !
  integer                   :: iorb_
  character(len=1)          :: char
  integer                   :: id
  !
  iorb_=1;if(present(iorb))iorb_=iorb
  char='Z';if(present(component))char=component
  select case(char)
  case default;id=3
  case('X','x');id=1
  case('Y','y');id=2
  end select
  if(iorb_>Norb)stop "ed_get_mag error: orbital index > N_orbital"
  self = ed_mag(id,iorb_)
end subroutine ed_get_mag_n0

subroutine ed_get_mag_n1(self,component,iorb,Nlat)
  real(8),dimension(:)     :: self
  character(len=1),optional :: component
  integer,optional          :: iorb,Nlat
  !
  integer                   :: iorb_
  character(len=1)          :: char
  integer                   :: id
  !
  iorb_=1;if(present(iorb))iorb_=iorb
  char='Z';if(present(component))char=component
  select case(char)
  case default;id=3
  case('X','x');id=1
  case('Y','y');id=2
  end select
  if(iorb_>Norb)stop "ed_get_mag error: orbital index > N_orbital"
  if(present(Nlat))then
     if(.not.allocated(mag_ineq))stop "ed_get_mag error: mag_ineq not allocated"
     if(Nlat>size(mag_ineq,1))stop "ed_get_mag error: required N_sites > evaluated N_sites"
  endif
  if(present(Nlat))then
     call assert_shape(self,[Nlat],'ed_get_mag','mag')
     self = mag_ineq(:,id,iorb_)
  else
     call assert_shape(self,[Norb],'ed_get_mag','mag')
     self = ed_mag(id,:)
  endif
end subroutine ed_get_mag_n1

subroutine ed_get_mag_n2(self,component,Nlat)
  real(8),dimension(:,:)    :: self
  character(len=1),optional :: component
  integer                   :: Nlat
  !
  character(len=1)          :: char
  integer                   :: id
  !
  char='Z';if(present(component))char=component
  select case(char)
  case default;id=3
  case('X','x');id=1
  case('Y','y');id=2
  end select
  if(.not.allocated(mag_ineq))stop "ed_get_mag error: mag_ineq not allocated"
  if(Nlat>size(mag_ineq,1))stop "ed_get_mag error: required N_sites > evaluated N_sites"
  call assert_shape(self,[Nlat,Norb],'ed_get_mag','mag')
  self = mag_ineq(:,id,:)
end subroutine ed_get_mag_n2

subroutine ed_get_mag_n3(self,component,Nlat)
  real(8),dimension(:,:,:)  :: self
  character(len=1),optional :: component
  integer                   :: Nlat
  !
  character(len=1)          :: char
  integer                   :: id
  !
  char='Z';if(present(component))char=component
  select case(char)
  case default;id=3
  case('X','x');id=1
  case('Y','y');id=2
  end select
  if(.not.allocated(mag_ineq))stop "ed_get_mag error: mag_ineq not allocated"
  if(Nlat>size(mag_ineq,1))stop "ed_get_mag error: required N_sites > evaluated N_sites"
  call assert_shape(self,[Nlat,3,Norb],'ed_get_mag','mag')
  self = mag_ineq
end subroutine ed_get_mag_n3
