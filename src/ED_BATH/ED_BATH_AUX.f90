MODULE ED_BATH_AUX
  USE SF_CONSTANTS, only: zero
  USE SF_IOTOOLS, only:free_unit,reg,file_length,str
  USE SF_LINALG, only: eye,inv
  USE SF_ARRAYS, only:linspace
  USE SF_MISC, only: assert_shape
  USE ED_INPUT_VARS
  USE ED_VARS_GLOBAL
  USE ED_AUX_FUNX
  implicit none


  !Aux:
  interface get_Whyb_matrix
     module procedure get_Whyb_matrix_1orb
     module procedure get_Whyb_matrix_Aorb
     module procedure get_Whyb_matrix_dmft_bath
  end interface get_Whyb_matrix

  interface is_identity
     module procedure ::  is_identity_so
     module procedure ::  is_identity_nn
  end interface is_identity

  interface is_diagonal
     module procedure ::  is_diagonal_so
     module procedure ::  is_diagonal_nn
  end interface is_diagonal




contains



  !##################################################################
  !
  !     HREPLICA AUX FUNCTIONS:
  !
  !##################################################################
  subroutine Hreplica_site(site)
    integer :: site
    if(site<1.OR.site>size(Hreplica_lambda_ineq,1))stop "ERROR Hreplica_site: site not in [1,Nlat]"
    if(.not.allocated(Hreplica_lambda_ineq))stop "ERROR Hreplica_site: Hreplica_lambda_ineq not allocated"
    Hreplica_lambda(:,:)  = Hreplica_lambda_ineq(site,:,:)
  end subroutine Hreplica_site


  !reconstruct [Nspin,Nspin,Norb,Norb] hamiltonian from basis expansion given [lambda]
  function Hreplica_build(lambdavec) result(H)
    real(8),dimension(:),optional                             :: lambdavec ![Nsym]
    real(8),dimension(:),allocatable                          :: lambda
    integer                                                   :: isym
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: H
    !
    if(.not.Hreplica_status)STOP "ERROR Hreplica_build: Hreplica_basis is not setup"
    allocate(lambda(size(Hreplica_basis)));lambda=1d0
    if(present(lambdavec))then
       if(size(lambdavec)/=size(Hreplica_basis)) STOP "ERROR Hreplica_build: Wrong coefficient vector size"
       lambda = lambdavec
    endif
    H=zero
    do isym=1,size(lambda)
       H=H+lambda(isym)*Hreplica_basis(isym)%O
    enddo
  end function Hreplica_build

  !Create bath mask
  function Hreplica_mask(wdiag,uplo) result(Hmask)
    logical,optional                                          :: wdiag,uplo
    logical                                                   :: wdiag_,uplo_
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: H
    logical,dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb)    :: Hmask
    integer                                                   :: iorb,jorb,ispin,jspin,io,jo
    !
    wdiag_=.false.;if(present(wdiag))wdiag_=wdiag
    uplo_ =.false.;if(present(uplo))  uplo_=uplo
    !
    H = Hreplica_build(Hreplica_lambda(Nbath,:)) !The mask should be replica-independent
    Hmask=.false.
    where(abs(H)>1d-6)Hmask=.true.
    !
    !
    if(wdiag_)then
       do ispin=1,Nnambu*Nspin
          do iorb=1,Norb
             Hmask(ispin,ispin,iorb,iorb)=.true.
          enddo
       enddo
    endif
    !
    if(uplo_)then
       do ispin=1,Nnambu*Nspin
          do jspin=1,Nnambu*Nspin
             do iorb=1,Norb
                do jorb=1,Norb
                   io = index_stride_so(ispin,iorb)
                   jo = index_stride_so(jspin,jorb)
                   if(io>jo)Hmask(ispin,jspin,iorb,jorb)=.false.
                enddo
             enddo
          enddo
       enddo
    endif
    !
  end function Hreplica_mask





  !##################################################################
  !
  !     HGENERAL AUX FUNCTIONS:
  !
  !##################################################################
  subroutine Hgeneral_site(site)
    integer :: site
    if(site<1.OR.site>size(Hgeneral_lambda_ineq,1))stop "ERROR Hgeneral_site: site not in [1,Nlat]"
    if(.not.allocated(Hgeneral_lambda_ineq))stop "ERROR Hgeneral_site: Hgeneral_lambda_ineq not allocated"
    Hgeneral_lambda(:,:)  = Hgeneral_lambda_ineq(site,:,:)
  end subroutine Hgeneral_site


  !reconstruct [Nspin,Nspin,Norb,Norb] hamiltonian from basis expansion given [lambda]
  function Hgeneral_build(lambdavec) result(H)
    real(8),dimension(:),optional                             :: lambdavec ![Nsym]
    real(8),dimension(:),allocatable                          :: lambda
    integer                                                   :: isym
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: H
    !
    if(.not.Hgeneral_status)STOP "ERROR Hgeneral_build: Hgeneral_basis is not setup"
    allocate(lambda(size(Hgeneral_basis)));lambda=1d0
    if(present(lambdavec))then
       if(size(lambdavec)/=size(Hgeneral_basis)) STOP "ERROR Hgeneral_build: Wrong coefficient vector size"
       lambda = lambdavec
    endif
    H=zero
    do isym=1,size(lambdavec)
       H=H+lambdavec(isym)*Hgeneral_basis(isym)%O
    enddo
  end function Hgeneral_build




  !+-------------------------------------------------------------------+
  !PURPOSE  : Create bath mask
  !+-------------------------------------------------------------------+
  function Hgeneral_mask(wdiag,uplo) result(Hmask)
    logical,optional                                          :: wdiag,uplo
    logical                                                   :: wdiag_,uplo_
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: H
    logical,dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb)    :: Hmask
    integer                                                   :: iorb,jorb,ispin,jspin,io,jo
    !
    wdiag_=.false.;if(present(wdiag))wdiag_=wdiag
    uplo_ =.false.;if(present(uplo))  uplo_=uplo
    !
    H = Hgeneral_build(Hgeneral_lambda(Nbath,:)) !The mask should be general-independent
    Hmask=.false.
    where(abs(H)>1d-6)Hmask=.true.
    !
    !
    if(wdiag_)then
       do ispin=1,Nnambu*Nspin
          do iorb=1,Norb
             Hmask(ispin,ispin,iorb,iorb)=.true.
          enddo
       enddo
    endif
    !
    if(uplo_)then
       do ispin=1,Nnambu*Nspin
          do jspin=1,Nnambu*Nspin
             do iorb=1,Norb
                do jorb=1,Norb
                   io = index_stride_so(ispin,iorb)
                   jo = index_stride_so(jspin,jorb)
                   if(io>jo)Hmask(ispin,jspin,iorb,jorb)=.false.
                enddo
             enddo
          enddo
       enddo
    endif
    !
  end function Hgeneral_mask



  !##################################################################
  !
  !     W_hyb PROCEDURES
  !
  !##################################################################
  !+-----------------------------------------------------------------------------+!
  !PURPOSE: build up the all-spin hybridization matrix W_{ss`}
  !+-----------------------------------------------------------------------------+!
  function get_Whyb_matrix_1orb(v,u) result(w)
    real(8),dimension(Nspin,Nbath)       :: v,u
    real(8),dimension(Nspin,Nspin,Nbath) :: w
    integer                              :: ispin
    !
    ! if(ed_para)then
    !    do ispin=1,Nspin
    !       w(ispin,ispin,:) = v(1,:)
    !    enddo
    !    w(1,Nspin,:) = u(1,:)
    !    w(Nspin,1,:) = u(1,:)
    ! else
    do ispin=1,Nspin
       w(ispin,ispin,:) = v(ispin,:)
    enddo
    w(1,Nspin,:) = u(1,:)
    w(Nspin,1,:) = u(2,:)
    ! endif
    !
  end function get_Whyb_matrix_1orb

  function get_Whyb_matrix_Aorb(v,u) result(w)
    real(8),dimension(Nspin,Norb,Nbath)       :: v,u
    real(8),dimension(Nspin,Nspin,Norb,Nbath) :: w
    integer                                   :: ispin
    !
    ! if(ed_para)then
    !    do ispin=1,Nspin
    !       w(ispin,ispin,:,:) = v(1,:,:)
    !    enddo
    !    w(1,Nspin,:,:) = u(1,:,:)
    !    w(Nspin,1,:,:) = u(1,:,:)
    ! else
    do ispin=1,Nspin
       w(ispin,ispin,:,:) = v(ispin,:,:)
    enddo
    w(1,Nspin,:,:) = u(1,:,:)
    w(Nspin,1,:,:) = u(2,:,:)
    ! endif
    !
  end function get_Whyb_matrix_Aorb

  function get_Whyb_matrix_dmft_bath(dmft_bath_) result(w)
    type(effective_bath)                      :: dmft_bath_
    real(8),dimension(Nspin,Nspin,Norb,Nbath) :: w
    integer                                   :: ispin
    !
    ! if(ed_para)then
    !    do ispin=1,Nspin
    !       w(ispin,ispin,:,:) = dmft_bath_%v(1,:,:)
    !    enddo
    !    w(1,Nspin,:,:) = dmft_bath_%u(1,:,:)
    !    w(Nspin,1,:,:) = dmft_bath_%u(1,:,:)
    ! else
    do ispin=1,Nspin
       w(ispin,ispin,:,:) = dmft_bath_%v(ispin,:,:)
    enddo
    w(1,Nspin,:,:) = dmft_bath_%u(1,:,:)
    w(Nspin,1,:,:) = dmft_bath_%u(2,:,:)
    ! endif
    !
  end function get_Whyb_matrix_dmft_bath




  !+-------------------------------------------------------------------+
  !PURPOSE  : Check if a matrix is the identity
  !+-------------------------------------------------------------------+
  function is_identity_nn(mnnn) result(flag)
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: mnnn
    real(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mtmp
    integer                                  :: i,j
    logical                                  :: flag
    !
    flag=.true.
    !
    mtmp=nn2so_reshape(mnnn,Nnambu*Nspin,Norb)
    !
    do i=1,Nnambu*Nspin*Norb-1
       if((mtmp(i,i).ne.mtmp(i+1,i+1)).or.(mtmp(i,i).lt.1.d-6))flag=.false.
    enddo
    !
    do i=1,Nnambu*Nspin*Norb
       do j=1,Nnambu*Nspin*Norb
          if((i.ne.j).and.(mtmp(i,j).gt.1.d-6))flag=.false.
       enddo
    enddo
    !
  end function is_identity_nn

  function is_identity_so(mlso) result(flag)
    complex(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mlso
    real(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mtmp
    integer                                  :: i,j
    logical                                  :: flag
    !
    flag=.true.
    !
    mtmp=mlso
    !
    do i=1,Nnambu*Nspin*Norb-1
       if((mtmp(i,i).ne.mtmp(i+1,i+1)).or.(mtmp(i,i).lt.1.d-6))flag=.false.
    enddo
    !
    do i=1,Nnambu*Nspin*Norb
       do j=1,Nnambu*Nspin*Norb
          if((i.ne.j).and.(mtmp(i,j).gt.1.d-6))flag=.false.
       enddo
    enddo
    !
  end function is_identity_so



  !+-------------------------------------------------------------------+
  !PURPOSE  : Check if a matrix is diagonal
  !+-------------------------------------------------------------------+
  function is_diagonal_nn(mnnn) result(flag)
    complex(8),dimension(Nnambu*Nspin,Nnambu*Nspin,Norb,Norb) :: mnnn
    complex(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mtmp
    integer                                     :: i,j
    logical                                     :: flag
    !
    flag=.true.
    !
    mtmp=abs((nn2so_reshape(mnnn,Nnambu*Nspin,Norb)))
    !
    do i=1,Nnambu*Nspin*Norb
       do j=1,Nnambu*Nspin*Norb
          if((i.ne.j).and.(abs(mtmp(i,j)).gt.1.d-6))flag=.false.
       enddo
    enddo
    !
  end function is_diagonal_nn

  function is_diagonal_so(mlso) result(flag)
    complex(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mlso
    complex(8),dimension(Nnambu*Nspin*Norb,Nnambu*Nspin*Norb) :: mtmp
    integer                                     :: i,j
    logical                                     :: flag
    !
    flag=.true.
    !
    mtmp=abs((mlso))
    !
    do i=1,Nnambu*Nspin*Norb
       do j=1,Nnambu*Nspin*Norb
          if((i.ne.j).and.(abs(mtmp(i,j)).gt.1.d-6))flag=.false.
       enddo
    enddo
    !
  end function is_diagonal_so






  function check_herm(A,N,error) result(bool)
    integer,intent(in)                   :: N
    complex(8),dimension(N,N),intent(in) :: A
    logical                              :: bool
    real(8),optional                     :: error
    real(8)                              :: error_
    error_ = 1d-6 ; if(present(error))error_=error
    bool   = all(abs(A - conjg(transpose(A)))<error_)
  end function check_herm


  function check_nambu(A,N,error) result(bool)
    integer,intent(in)                       :: N
    complex(8),dimension(2*N,2*N),intent(in) :: A
    complex(8),dimension(N,N)                :: h11,h22
    logical                                  :: bool
    real(8),optional                         :: error
    real(8)                                  :: error_
    error_ = 1d-6 ; if(present(error))error_=error
    h11    = A(1:N    ,1:N)
    h22    = A(N+1:2*N,N+1:2*N)
    bool   = check_herm(A,2*N,error_) !this checks also for F = A_12, s.t. A_21=herm(A_12)
    bool   = bool.AND.( all(abs(h22 + conjg(h11))<error_) )
  end function check_nambu





END MODULE ED_BATH_AUX










! public :: get_bath_component_dimension
! public :: get_bath_component
! public :: set_bath_component
! public :: copy_bath_component
! !



! !+-------------------------------------------------------------------+
! !PURPOSE  : Inquire the correct bath size to allocate the
! ! the bath array in the calling program.
! !
! ! Get size of each dimension of the component array.
! ! The Result is an rank 1 integer array Ndim with dimension:
! ! 3 for get_component_size_bath
! ! 2 for get_spin_component_size_bath & get_orb_component_size_bath
! ! 1 for get_spin_orb_component_size_bath
! !+-------------------------------------------------------------------+
! function get_bath_component_dimension(type) result(Ndim)
!   character(len=1) :: type
!   integer          :: Ndim(3)
!   call  check_bath_component(type)
!   select case(bath_type)
!   case default
!      Ndim=[Nspin,Norb,Nbath]
!   case('hybrid')
!      select case(ed_mode)
!      case default
!         select case(type)
!         case('e')
!            Ndim=[Nspin,1,Nbath]
!         case('v')
!            Ndim=[Nspin,Norb,Nbath]
!         end select
!      case ("superc")
!         select case(type)
!         case('e','d')
!            Ndim=[Nspin,1,Nbath]
!         case('v')
!            Ndim=[Nspin,Norb,Nbath]
!         end select
!      case ("nonsu2")
!         select case(type)
!         case('e')
!            Ndim=[Nspin,1,Nbath]
!         case('v','u')
!            Ndim=[Nspin,Norb,Nbath]
!         end select
!      end select
!   end select
! end function get_bath_component_dimension


! !+-----------------------------------------------------------------------------+!
! !PURPOSE: check that the input array hsa the correct dimensions specified
! ! for the choice of itype and possiblty ispin and/or iorb.
! !+-----------------------------------------------------------------------------+!
! subroutine assert_bath_component_size(array,type,string1,string2)
!   real(8),dimension(:,:,:) :: array
!   character(len=1)         :: type
!   character(len=*)         :: string1,string2
!   integer                  :: Ndim(3)
!   Ndim = get_bath_component_dimension(type)
!   call assert_shape(Array,Ndim,reg(string1),reg(string2))
! end subroutine assert_bath_component_size








! !+-----------------------------------------------------------------------------+!
! !PURPOSE: Get a specified itype,ispin,iorb component of the user bath.
! ! The component is returned into an Array of rank D
! ! get_full_component_bath    : return the entire itype component (D=3)
! ! get_spin_component_bath    : return the itype component for the select ispin (D=2)
! ! get_spin_orb_component_bath: return the itype component for the select ispin & iorb (D=1)
! !+-----------------------------------------------------------------------------+!
! subroutine get_bath_component(array,bath_,type)
!   real(8),dimension(:,:,:) :: array
!   real(8),dimension(:)     :: bath_
!   character(len=1)         :: type
!   logical                  :: check
!   type(effective_bath)     :: dmft_bath_
!   !
!   check= check_bath_dimension(bath_)
!   if(.not.check)stop "get_component_bath error: wrong bath dimensions"
!   call allocate_dmft_bath(dmft_bath_)
!   call set_dmft_bath(bath_,dmft_bath_)
!   call assert_bath_component_size(array,type,"get_bath_component","Array")
!   call check_bath_component(type)
!   select case(ed_mode)
!   case default
!      select case(type)
!      case('e')
!         Array = dmft_bath_%e(:,:,:)
!      case('v')
!         Array = dmft_bath_%v(:,:,:)
!      end select
!   case ("superc")
!      select case(type)
!      case('e')
!         Array = dmft_bath_%e(:,:,:)
!      case('d')
!         Array = dmft_bath_%d(:,:,:)
!      case('v')
!         Array = dmft_bath_%v(:,:,:)
!      end select
!   case ("nonsu2")
!      select case(type)
!      case('e')
!         Array = dmft_bath_%e(:,:,:)
!      case('v')
!         Array = dmft_bath_%v(:,:,:)
!      case('u')
!         Array = dmft_bath_%u(:,:,:)
!      end select
!   end select
!   call deallocate_dmft_bath(dmft_bath_)
! end subroutine get_bath_component


! !+-----------------------------------------------------------------------------+!
! !PURPOSE: Set a specified itype,ispin,iorb component of the user bath.
! !+-----------------------------------------------------------------------------+!
! subroutine set_bath_component(array,bath_,type)
!   real(8),dimension(:,:,:) :: array
!   real(8),dimension(:)     :: bath_
!   character(len=1)         :: type
!   logical                  :: check
!   type(effective_bath)     :: dmft_bath_
!   !
!   check= check_bath_dimension(bath_)
!   if(.not.check)stop "set_component_bath error: wrong bath dimensions"
!   call allocate_dmft_bath(dmft_bath_)
!   call set_dmft_bath(bath_,dmft_bath_)
!   call assert_bath_component_size(array,type,"set_bath_component","Array")
!   call check_bath_component(type)
!   select case(ed_mode)
!   case default
!      select case(type)
!      case('e')
!         dmft_bath_%e(:,:,:) = Array
!      case('v')
!         dmft_bath_%v(:,:,:) = Array
!      end select
!   case ("superc")
!      select case(type)
!      case('e')
!         dmft_bath_%e(:,:,:) = Array
!      case('d')
!         dmft_bath_%d(:,:,:) = Array
!      case('v')
!         dmft_bath_%v(:,:,:) = Array
!      end select
!   case ("nonsu2")
!      select case(type)
!      case('e')
!         dmft_bath_%e(:,:,:) = Array
!      case('v')
!         dmft_bath_%v(:,:,:) = Array
!      case('u')
!         dmft_bath_%u(:,:,:) = Array
!      end select
!   end select
!   call get_dmft_bath(dmft_bath_,bath_)
!   call deallocate_dmft_bath(dmft_bath_)
! end subroutine set_bath_component



! !+-----------------------------------------------------------------------------+!
! !PURPOSE: Copy a specified component of IN bath to the OUT bath.
! !+-----------------------------------------------------------------------------+!
! subroutine copy_bath_component(bathIN,bathOUT,type)
!   real(8),dimension(:)     :: bathIN,bathOUT
!   character(len=1)         :: type
!   logical                  :: check
!   type(effective_bath)     :: dIN,dOUT
!   !
!   check= check_bath_dimension(bathIN)
!   if(.not.check)stop "copy_component_bath error: wrong bath dimensions IN"
!   check= check_bath_dimension(bathOUT)
!   if(.not.check)stop "copy_component_bath error: wrong bath dimensions OUT"
!   call allocate_dmft_bath(dIN)
!   call allocate_dmft_bath(dOUT)
!   call set_dmft_bath(bathIN,dIN)
!   call set_dmft_bath(bathOUT,dOUT)
!   call check_bath_component(type)
!   select case(ed_mode)
!   case default
!      select case(type)
!      case('e')
!         dOUT%e(:,:,:)  = dIN%e(:,:,:)
!      case('v')
!         dOUT%v(:,:,:)  = dIN%v(:,:,:)
!      end select
!   case ("superc")
!      select case(type)
!      case('e')
!         dOUT%e(:,:,:)  = dIN%e(:,:,:)
!      case('d')
!         dOUT%d(:,:,:)  = dIN%d(:,:,:)
!      case('v')
!         dOUT%v(:,:,:)  = dIN%v(:,:,:)
!      end select
!   case ("nonsu2")
!      select case(type)
!      case('e')
!         dOUT%e(:,:,:)  = dIN%e(:,:,:)
!      case('v')
!         dOUT%v(:,:,:)  = dIN%v(:,:,:)
!      case('u')
!         dOUT%u(:,:,:)  = dIN%u(:,:,:)
!      end select
!   end select
!   call get_dmft_bath(dOUT,bathOUT)
!   call deallocate_dmft_bath(dIN)
!   call deallocate_dmft_bath(dOUT)
! end subroutine copy_bath_component
