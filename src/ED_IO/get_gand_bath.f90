subroutine ed_get_g0and_n3(x,bath_,G0and,axis,type)
  complex(8),dimension(:),intent(in)                  :: x
  real(8),dimension(:)                                :: bath_
  complex(8),dimension(:,:,:)                            :: G0and
  character(len=*),optional                           :: axis
  character(len=*),optional                           :: type
  !
  type(effective_bath)                                :: dmft_bath_
  logical                                             :: check
  character(len=1)                                    :: axis_,type_
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(x)) :: g0
  integer                                             :: L
  !
  axis_='m';if(present(axis))axis_=axis
  type_='n';if(present(type))type_=trim(type)
  check= check_bath_dimension(bath_)
  if(.not.check)stop "g0and_bath_mats_main_ error: wrong bath dimensions"
  call allocate_dmft_bath(dmft_bath_)
  call set_dmft_bath(bath_,dmft_bath_)
  select case(type_)
  case default;stop "ed_get_g0and ERROR: type is wrong: either Normal or Anomalous"
  case ('n','N')
     g0 = g0and_bath_function(x,dmft_bath_)
  case('a','A')
     g0 = f0and_bath_function(x,dmft_bath_)
  end select
  call deallocate_dmft_bath(dmft_bath_)
  !
  L=size(x)
  call assert_shape(g0and,[Nspin*Norb,Nspin*Norb,L],'ed_get_g0and','g0and')
  g0and = nn2so_reshape(g0,Nspin,Norb,L)
end subroutine ed_get_g0and_n3

subroutine ed_get_g0and_n5(x,bath_,G0and,axis,type)
  complex(8),dimension(:),intent(in)                  :: x
  real(8),dimension(:)                                :: bath_
  complex(8),dimension(:,:,:,:,:)                     :: G0and
  character(len=*),optional                           :: axis
  character(len=*),optional                           :: type
  !
  type(effective_bath)                                :: dmft_bath_
  logical                                             :: check
  character(len=1)                                    :: axis_,type_
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(x)) :: g0
  integer                                             :: L
  !
  axis_='m';if(present(axis))axis_=axis
  type_='n';if(present(type))type_=trim(type)
  check= check_bath_dimension(bath_)
  if(.not.check)stop "g0and_bath_mats_main_ error: wrong bath dimensions"
  call allocate_dmft_bath(dmft_bath_)
  call set_dmft_bath(bath_,dmft_bath_)
  select case(type_)
  case default;stop "ed_get_g0and ERROR: type is wrong: either Normal or Anomalous"
  case ('n','N')
     g0 = g0and_bath_function(x,dmft_bath_)
  case('a','A')
     g0 = f0and_bath_function(x,dmft_bath_)
  end select
  call deallocate_dmft_bath(dmft_bath_)
  !
  L=size(x)
  call assert_shape(g0and,[Nspin,Nspin,Norb,Norb,L],'ed_get_g0and','g0and')
  g0and = g0
end subroutine ed_get_g0and_n5






subroutine ed_get_delta_n3(x,bath_,delta,axis,type)
  complex(8),dimension(:),intent(in)                  :: x
  real(8),dimension(:)                                :: bath_
  complex(8),dimension(:,:,:)                            :: delta
  character(len=*),optional                           :: axis
  character(len=*),optional                           :: type
  !
  type(effective_bath)                                :: dmft_bath_
  logical                                             :: check
  character(len=1)                                    :: axis_,type_
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(x)) :: d0
  integer                                             :: L
  !
  axis_='mats';if(present(axis))axis_=axis
  check= check_bath_dimension(bath_)
  if(.not.check)stop "delta_bath_mats_main_ error: wrong bath dimensions"
  call allocate_dmft_bath(dmft_bath_)
  call set_dmft_bath(bath_,dmft_bath_)
  select case(type_)
  case default;stop "ed_get_delta ERROR: type is wrong: either Normal or Anomalous"
  case ('n','N')
     d0 = delta_bath_function(x,dmft_bath_,axis_)
  case('a','A')
     d0 = fdelta_bath_function(x,dmft_bath_,axis_)
  end select
  call deallocate_dmft_bath(dmft_bath_)
  !
  L=size(x)
  call assert_shape(delta,[Nspin*Norb,Nspin*Norb,L],'ed_get_delta','delta')
  delta = nn2so_reshape(d0,Nspin,Norb,L)
end subroutine ed_get_delta_n3

subroutine ed_get_delta_n5(x,bath_,delta,axis,type)
  complex(8),dimension(:),intent(in)                  :: x
  real(8),dimension(:)                                :: bath_
  complex(8),dimension(:,:,:,:,:)                            :: delta
  character(len=*),optional                           :: axis
  character(len=*),optional                           :: type
  !
  type(effective_bath)                                :: dmft_bath_
  logical                                             :: check
  character(len=1)                                    :: axis_,type_
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(x)) :: d0
  integer                                             :: L
  !
  axis_='mats';if(present(axis))axis_=axis
  check= check_bath_dimension(bath_)
  if(.not.check)stop "delta_bath_mats_main_ error: wrong bath dimensions"
  call allocate_dmft_bath(dmft_bath_)
  call set_dmft_bath(bath_,dmft_bath_)
  select case(type_)
  case default;stop "ed_get_delta ERROR: type is wrong: either Normal or Anomalous"
  case ('n','N')
     d0 = delta_bath_function(x,dmft_bath_,axis_)
  case('a','A')
     d0 = fdelta_bath_function(x,dmft_bath_,axis_)
  end select
  call deallocate_dmft_bath(dmft_bath_)
  !
  L=size(x)
  call assert_shape(delta,[Nspin,Nspin,Norb,Norb,L],'ed_get_delta','delta')
  delta = d0
end subroutine ed_get_delta_n5





