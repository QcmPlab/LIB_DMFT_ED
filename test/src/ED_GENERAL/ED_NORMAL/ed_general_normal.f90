program ed_general_normal
  USE EDIPACK2
  USE SCIFOR
  USE MPI
  USE SF_MPI
  USE ASSERTING
  implicit none
  integer                                     :: i,iw,jo,js,Nso,Nsymm,Nmomenta
  integer                                     :: unit,unit_
  real(8)                                     :: w,Re,Im
  !Bath:
  integer                                     :: Nb,iorb,jorb,ispin,jspin,inso,print_mode
  real(8),allocatable                         :: Bath(:),Wlist(:)
  !GFs and Sigma:
  complex(8),allocatable                      :: Weiss(:,:,:,:,:,:)
  complex(8),allocatable                      :: Smats(:,:,:,:,:)
  !hamiltonian input:
  complex(8),allocatable                      :: Hloc(:,:,:,:)
  real(8),dimension(:),allocatable            :: H0     ![Nso]
  !variables for the model:
  real(8)                                     :: Delta
  character(len=16)                           :: finput
  !General variables:
  real(8),allocatable                         :: dens(:),docc(:),exciton(:),energy(:),imp(:),Smats11mom(:),Smats12mom(:)
  !CHECK variables
  real(8),allocatable                         :: dens_(:),docc_(:),exciton_(:),energy_(:),imp_(:),Smats11mom_(:),Smats12mom_(:)
  !
  complex(8),dimension(4,4)                   :: GammaN,GammaE0
  real(8),dimension(:,:),allocatable          :: lambdasym_vector
  complex(8),dimension(:,:,:,:,:),allocatable :: Hsym_basis
  !MPI Vars:
  integer                                     :: irank,comm,rank,size2,ierr
  logical                                     :: master
  !
  ! MPI initialization
  call init_MPI()
  comm = MPI_COMM_WORLD
  call StartMsg_MPI(comm)
  rank = get_Rank_MPI(comm)
  size2 = get_Size_MPI(comm)
  master = get_Master_MPI(comm)
  !
  !Parse additional variables && read Input
  call parse_cmd_variable(finput,"FINPUT",default="inputED.in")
  call parse_input_variable(delta,"DELTA",finput,default=0.d0)
  !
  !
  call ed_read_input(trim(finput))
  !
  if(bath_type/="general")stop "Wrong setup from input file: non general bath"
  if(Norb/=2)stop "Wrong setup from input file: Norb!=2"
  if(Nspin/=1 )stop "Wrong setup from input file: Nspin/=1"
  Nso=Nspin*Norb
  Nmomenta=4
  !Allocate Weiss Field:
  allocate(Weiss(1,Nspin,Nspin,Norb,Norb,Lmats))
  allocate(Smats(Nspin,Nspin,Norb,Norb,Lmats))
  !
  ! Matrices for general hamiltonian
  gammaN =kron( pauli_sigma_0, pauli_tau_0)
  gammaE0=kron( pauli_sigma_0, pauli_tau_x )
  !
  allocate(Hloc(Nspin,Nspin,Norb,Norb))
  allocate(H0(Nso))
  Hloc = zero
  H0   = zero
  do js=1,Nspin
     Hloc(js,js,:,:)= Delta*pauli_sigma_z
     do jo=1,Norb
        H0(jo+2*(js-1)) =Hloc(js,js,jo,jo)
     end do
  end do
  !
  print_mode=3
  !
  ! Set up general hamiltonian
  Nsymm=2
  allocate(lambdasym_vector(Nbath,Nsymm))
  allocate(Hsym_basis(Nspin,Nspin,Norb,Norb,Nsymm))
  !
  ! N
  Hsym_basis(:,:,:,:,1)=j2so(GammaN(:Nso,:Nso))
  do i=1,Nbath
     lambdasym_vector(i,1) = -1.0 + 2.0*dble(i-1)/dble(Nbath-1)
  end do
  !
  ! E0
  Hsym_basis(:,:,:,:,2)=j2so(GammaE0(:Nso,:Nso))
  lambdasym_vector(:,2)=0.1d0
  !
  call ed_set_Hgeneral(Hsym_basis,lambdasym_vector)
  Nb=ed_get_bath_dimension(Nsymm)
  allocate(Bath(Nb))
  call ed_init_solver(bath)
  !
  !
  !set Hloc
  call ed_set_Hloc(hloc)
  !
  !Solve the IMPURITY PROBLEM
  call ed_solve(bath)
  call ed_get_sigma(Smats,axis="m",type="n")
  !
  !
  ! Check observables
  allocate(dens(Norb),dens_(Norb))
  allocate(docc(Norb),docc_(Norb))
  allocate(exciton(2),exciton_(2))
  allocate(energy(8),energy_(8))
  allocate(imp(4),imp_(4))
  allocate(Wlist(size(Smats,5)))
  allocate(Smats11mom(Nmomenta),Smats11mom_(Nmomenta))
  allocate(Smats12mom(Nmomenta),Smats12mom_(Nmomenta))
  write(*,*) ""
  write(*,*) "ED_MODE = NORMAL   |   BATH_TYPE = GENERAL"
  write(*,*) "Checking..."
  ! density
  unit =free_unit()
  unit_=free_unit()
  open(unit,file="dens_last.ed")
  read(unit,*) dens(:)
  close(unit)
  open(unit_,file="dens_last.check")
  read(unit_,*) dens_(:)
  close(unit_)
  call assert(dens,dens_,"dens(:)")
  ! double occupancy
  open(unit,file="docc_last.ed")
  read(unit,*) docc(:)
  close(unit)
  open(unit_,file="docc_last.check")
  read(unit_,*) docc_(:)
  close(unit_)
  call assert(docc,docc_,"docc(:)")
  ! Exciton Order Parameters
  open(unit,file="exciton_last.ed")
  read(unit,*) exciton(:)
  close(unit)
  open(unit_,file="exciton_last.check")
  read(unit_,*) exciton_(:)
  close(unit_)
  call assert(exciton,exciton_,"exciton(:)")
  ! Energies
  open(unit,file="energy_last.ed")
  read(unit,*) energy(:)
  close(unit)
  open(unit_,file="energy_last.check")
  read(unit_,*) energy_(:)
  close(unit_)
  call assert(energy,energy_,"energy(:)")
  ! Impurity
  open(unit,file="imp_last.ed")
  read(unit,*) imp(:)
  close(unit)
  open(unit_,file="imp_last.check")
  read(unit_,*) imp_(:)
  close(unit_)
  call assert(imp,imp_,"imp(:)")
  ! Self-Energies
  open(unit,file="impSigma_l11_s1_iw.ed")
  do iw=1,size(Smats,5)
     read(unit,*) Wlist(iw), Im, Re
  end do
  close(unit)
  ! Get momenta
  do i=1,Nmomenta
     call compute_momentum(Wlist,Smats(1,1,1,1,:),i,Smats11mom(i))
     call compute_momentum(Wlist,Smats(1,1,1,2,:),i,Smats12mom(i))
  enddo
  ! Write new momenta
  open(unit_,file="impSigma_l11_s1_iw.momenta.new")
  do i=1,Nmomenta
     write(unit_,*) i, Smats11mom(i)
  enddo
  close(unit_)
  open(unit_,file="impSigma_l12_s1_iw.momenta.new")
  do i=1,Nmomenta
     write(unit_,*) i, Smats12mom(i)
  enddo
  close(unit_)
  !Read check momenta
  open(unit_,file="impSigma_l11_s1_iw.momenta.check")
  do i=1,Nmomenta
     read(unit_,*) iw, Smats11mom_(i)
  end do
  close(unit_)
  open(unit_,file="impSigma_l12_s1_iw.momenta.check")
  do i=1,Nmomenta
     read(unit_,*) iw, Smats12mom_(i)
  end do
  close(unit_)
  call assert(Smats11mom/Smats11mom_,dble(ones(Nmomenta)),"Sigma_matsubara_l11(:)",tol=1.0d-8)
  call assert(Smats12mom/Smats12mom_,dble(ones(Nmomenta)),"Sigma_matsubara_l12(:)",tol=1.0d-8)
  
  
  call finalize_MPI()



contains


  function so2j_index(ispin,iorb) result(isporb)
    integer :: ispin,iorb
    integer :: isporb
    if(iorb>Norb)stop "error so2j_index: iorb>Norb"
    if(ispin>Nspin)stop "error so2j_index: ispin>Nspin"
    isporb=(ispin-1)*Nspin + iorb
  end function so2j_index


  function so2j(fg) result(g)
    complex(8),dimension(Nspin,Nspin,Norb,Norb) :: fg
    complex(8),dimension(Nspin*Norb,Nspin*Norb) :: g
    integer                                     :: i,j,iorb,jorb,ispin,jspin
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                i=so2j_index(ispin,iorb)
                j=so2j_index(jspin,jorb)
                g(i,j) = fg(ispin,jspin,iorb,jorb)
             enddo
          enddo
       enddo
    enddo
  end function so2j

  function j2so(fg) result(g)
    complex(8),dimension(Nspin*Norb,Nspin*Norb) :: fg
    complex(8),dimension(Nspin,Nspin,Norb,Norb) :: g
    integer                                     :: i,j,iorb,jorb,ispin,jspin
    do ispin=1,Nspin
       do jspin=1,Nspin
          do iorb=1,Norb
             do jorb=1,Norb
                i=so2j_index(ispin,iorb)
                j=so2j_index(jspin,jorb)
                g(ispin,jspin,iorb,jorb)  = fg(i,j)
             enddo
          enddo
       enddo
    enddo
  end function j2so

  
  ! Subroutine to compute momenta
  ! 
  ! ( sum_w abs(F(w))*w**n ) / ( sum_w abs(F(w)) )
  subroutine compute_momentum(x,Fx,n,momentum)
    real(8)   ,dimension(:),intent(in)       :: x
    complex(8),dimension(:),intent(in)       :: Fx
    integer   ,intent(in)                    :: n
    real(8)   ,intent(out)                   :: momentum
    !
    integer                                  :: iw
    real(8)                                  :: num,den
    num=0.0;den=0.0
    do iw=1,size(x,1)
       num = num + abs(Fx(iw))*x(iw)**n
       den = den + abs(Fx(iw))
    enddo
    momentum=num/den
  end subroutine compute_momentum
  
  
end program ed_general_normal
