subroutine ed_get_docc_n0(self,iorb)
  real(8)          :: self
  integer,optional :: iorb
  integer          :: iorb_
  iorb_=1;if(present(iorb))iorb_=iorb
  if(iorb_>Norb)stop "ed_get_docc error: orbital index > N_orbital"
  self = ed_docc(iorb_)
end subroutine ed_get_docc_n0

subroutine ed_get_docc_n1(self,iorb,Nlat)
  real(8),dimension(:) :: self
  integer,optional     :: iorb,Nlat
  integer              :: iorb_
  iorb_=1;if(present(iorb))iorb_=iorb
  if(iorb_>Norb)stop "ed_get_docc error: orbital index > N_orbital"
  if(present(Nlat))then
     if(.not.allocated(docc_ineq))stop "ed_get_docc error: docc_ineq not allocated"
     if(Nlat>size(docc_ineq,1))stop "ed_get_docc error: required N_sites > evaluated N_sites"
  endif
  if(present(Nlat))then
     call assert_shape(self,[Nlat],'ed_get_docc','docc')
     self = docc_ineq(:,iorb_)
  else
     call assert_shape(self,[Norb],'ed_get_docc','docc')
     self = ed_docc
  endif
end subroutine ed_get_docc_n1

subroutine ed_get_docc_n2(self,Nlat)
  real(8),dimension(:,:) :: self
  integer                :: Nlat
  if(.not.allocated(docc_ineq))stop "ed_get_docc error: docc_ineq not allocated"
  if(Nlat>size(docc_ineq,1))stop "ed_get_docc error: required N_sites > evaluated N_sites"
  call assert_shape(self,[Nlat,Norb],'ed_get_docc','docc')
  self = docc_ineq
end subroutine ed_get_docc_n2
