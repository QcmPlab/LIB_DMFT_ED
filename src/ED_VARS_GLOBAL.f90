MODULE ED_VARS_GLOBAL
  USE SF_CONSTANTS
  USE SF_IOTOOLS, only:free_unit,reg,str
  USE ED_SPARSE_MATRIX
  USE ED_INPUT_VARS, only:ed_verbose,logfile
#ifdef _MPI
  USE MPI
  USE SF_MPI
#endif
  implicit none


  !-------------------- EFFECTIVE BATH STRUCTURE ----------------------!
  type H_operator
     complex(8),dimension(:,:,:,:),allocatable :: O !Replica/General hamiltonian
  end type H_operator

  type effective_bath_component
     real(8)                                   :: v
     real(8),dimension(:),allocatable          :: vg
     real(8),dimension(:),allocatable          :: lambda ![Nsym]
  end type effective_bath_component

  type effective_bath
     !normal
     real(8),dimension(:,:,:),allocatable      :: e !local energies [Nspin][Norb][Nbath]/[Nspin][1][Nbath]_hybrid
     real(8),dimension(:,:,:),allocatable      :: v !spin-keep hyb. [Nspin][Norb][Nbath]
     !superc
     real(8),dimension(:,:,:),allocatable      :: d !SC amplitues   [Nspin][Norb][Nbath]/[Nspin][1][Nbath]_hybrid
     !nonsu2
     real(8),dimension(:,:,:),allocatable      :: u !spin-flip hyb. [Nspin][Norb][Nbath]
     !replica/general
     integer                                                 :: Nbasis  !H Basis dimension     
     type(effective_bath_component),dimension(:),allocatable :: item    ![Nbath] Replica/General bath components, V included
     !
     logical                                                 :: status=.false.
  end type effective_bath



  !-------------------- CUSTOM OBSERVABLE STRUCTURE ----------------------!
  type observable
     complex(8),dimension(:,:,:),allocatable   :: sij ![Nlso][Nlso][Nk]
     character(len=32)                         :: o_name
     real(8)                                   :: o_value
  end type observable

  type custom_observables
     type(observable),dimension(:),allocatable :: item     ![:]
     complex(8),dimension(:,:,:),allocatable   :: Hk       ![Nlso][Nlso][Nk]
     integer                                   :: N_asked
     integer                                   :: N_filled
     logical                                   :: init=.false.
  end type custom_observables




  !---------------- SECTOR-TO-FOCK SPACE STRUCTURE -------------------!
  type sector_map
     integer,dimension(:),allocatable          :: map
     logical                                   :: status=.false.
  end type sector_map

  type sector
     integer                                   :: index       !
     type(sector_map),dimension(:),allocatable :: H
     integer,dimension(:),allocatable          :: DimUps
     integer,dimension(:),allocatable          :: DimDws
     integer                                   :: DimUp
     integer                                   :: DimDw
     integer                                   :: DimEl
     integer                                   :: DimPh
     integer                                   :: Dim
     integer,dimension(:),allocatable          :: Nups
     integer,dimension(:),allocatable          :: Ndws
     integer                                   :: Nup
     integer                                   :: Ndw
     integer                                   :: Sz
     integer                                   :: Ntot,twoJz
     integer                                   :: Nlanc
     logical                                   :: status=.false.
  end type sector



  !-------------- GMATRIX FOR FAST EVALUATION OF GF ------------------!
  !The contributions to the GF Kallen-Lehmann sum are stored as
  !GF_{ab,sr}%state%channel%{w,e}.
  !A couple of weight,poles {w,e} is stored for each *channel, corresponding to c,cdg or any
  !their combination thereof as well as for any state |n> of the spectrum such that
  !GF(z) = sum w/z-e
  type GFspectrum
     real(8),dimension(:),allocatable       :: weight
     real(8),dimension(:),allocatable       :: poles
  end type GFspectrum

  !N_channel = c,cdag,c \pm cdag, c \pm i*cdag, ...
  type GFchannel
     type(GFspectrum),dimension(:),allocatable :: channel 
  end type GFchannel

  !state_list%size = # of state in the spectrum 
  type GFmatrix
     type(GFchannel),dimension(:),allocatable  :: state
     logical                                   :: status=.false.
  end type GFmatrix


  interface allocate_GFmatrix
     module procedure :: allocate_GFmatrix_Nstate
     module procedure :: allocate_GFmatrix_Nchan
     module procedure :: allocate_GFmatrix_Nexc
  end interface allocate_GFmatrix


  interface deallocate_GFmatrix
     module procedure :: deallocate_GFmatrix_single
     module procedure :: deallocate_GFmatrix_all1
     module procedure :: deallocate_GFmatrix_all2
     module procedure :: deallocate_GFmatrix_all3
     module procedure :: deallocate_GFmatrix_all4
  end interface deallocate_GFmatrix

  interface write_GFmatrix
     module procedure :: write_GFmatrix_single
     module procedure :: write_GFmatrix_all1
     module procedure :: write_GFmatrix_all2
     module procedure :: write_GFmatrix_all3
     module procedure :: write_GFmatrix_all4
  end interface write_GFmatrix

  interface read_GFmatrix
     module procedure :: read_GFmatrix_single
     module procedure :: read_GFmatrix_all1
     module procedure :: read_GFmatrix_all2
     module procedure :: read_GFmatrix_all3
     module procedure :: read_GFmatrix_all4
  end interface read_GFmatrix


  !------------------ ABTRACT INTERFACES PROCEDURES ------------------!
  !SPARSE MATRIX-VECTOR PRODUCTS USED IN ED_MATVEC
  !dbleMat*dbleVec
  abstract interface
     subroutine dd_sparse_HxV(Nloc,v,Hv)
       integer                 :: Nloc
       real(8),dimension(Nloc) :: v
       real(8),dimension(Nloc) :: Hv
     end subroutine dd_sparse_HxV
  end interface


  !cmplxMat*cmplxVec
  abstract interface
     subroutine cc_sparse_HxV(Nloc,v,Hv)
       integer                    :: Nloc
       complex(8),dimension(Nloc) :: v
       complex(8),dimension(Nloc) :: Hv
     end subroutine cc_sparse_HxV
  end interface




  !-------------------------- ED  VARIABLES --------------------------!

  !SIZE OF THE PROBLEM
  !=========================================================
  integer,save                                       :: Ns       !Number of levels per spin
  integer                                            :: Nlevels
  integer,save                                       :: Nsectors !Number of sectors
  integer,save                                       :: Ns_orb
  integer,save                                       :: Ns_ud
  integer                                            :: Nhel
  integer,save                                       :: DimPh    !Number of phonon states

  !Some maps between sectors and full Hilbert space (pointers)
  !PRIVATE:
  !=========================================================
  integer,allocatable,dimension(:)                   :: getDim             ! [Nsectors]
  integer,allocatable,dimension(:,:,:)               :: getCsector         ! [1/Norb,2,NSectors]
  integer,allocatable,dimension(:,:,:)               :: getCDGsector       ! [1/Norb,2,NSectors]
  integer,allocatable,dimension(:,:,:)               :: getCsector_Jz
  integer,allocatable,dimension(:,:,:)               :: getCDGsector_Jz
  integer,allocatable,dimension(:,:)                 :: getBathStride
  integer,allocatable,dimension(:,:)                 :: impIndex
  logical,allocatable,dimension(:)                   :: twin_mask
  logical,allocatable,dimension(:)                   :: sectors_mask
  integer,allocatable,dimension(:,:)                 :: getSector
  integer,allocatable,dimension(:)                   :: getSz
  integer,allocatable,dimension(:)                   :: getN
  integer,allocatable,dimension(:)                   :: gettwoJz
  integer,allocatable,dimension(:)                   :: getmaxtwoJz


  !Effective Bath used in the ED code (this is opaque to user)
  !=========================================================
  type(effective_bath)                               :: dmft_bath


  !Global Nambu factor for SC calculations (Nspin=1 but this index is 2 to
  !correctly allocate  Nambu arrays of dim 2*Norb) 
  !=========================================================
  integer                                            :: Nnambu=1
  !Replica/General bath basis set
  !=========================================================
  type(H_operator),dimension(:),allocatable          :: Hreplica_basis   ![Nsym]
  real(8),dimension(:,:),allocatable                 :: Hreplica_lambda  ![Nbath,Nsym]
  logical                                            :: Hreplica_status=.false.
  !
  type(H_operator),dimension(:),allocatable          :: Hgeneral_basis   ![Nsym]
  real(8),dimension(:,:),allocatable                 :: Hgeneral_lambda  ![Nbath,Nsym]
  logical                                            :: Hgeneral_status=.false.

  !local part of the Hamiltonian
  !=========================================================
  complex(8),dimension(:,:,:,:),allocatable          :: impHloc           !local hamiltonian



  !Variables for DIAGONALIZATION
  !PRIVATE
  !=========================================================  
  type(sparse_matrix_csr)                            :: spH0d !diagonal part
  type(sparse_matrix_csr)                            :: spH0nd !non-diagonal part
  type(sparse_matrix_csr),dimension(:),allocatable   :: spH0ups,spH0dws !reduced UP and DW parts
  type(sparse_matrix_csr)                            :: spH0_ph !Hamiltonian for phonons
  type(sparse_matrix_csr)                            :: spH0e_eph, spH0ph_eph !electron-phonon interaction
  type(sparse_matrix_csr)                            :: spH0
  procedure(dd_sparse_HxV),pointer                   :: spHtimesV_p=>null()
  procedure(cc_sparse_HxV),pointer                   :: spHtimesV_cc=>null()




  !Variables for DIAGONALIZATION
  !PRIVATE
  !=========================================================  
  integer,allocatable,dimension(:)                   :: neigen_sector
  logical                                            :: trim_state_list=.false.

  !Partition function
  !PRIVATE
  !=========================================================
  real(8)                                            :: zeta_function
  real(8)                                            :: gs_energy



  !Impurity Green's function and Self-Energies: (Nspin,Nspin,Norb,Norb,:)
  !Impurity Green's function for phonons
  !PRIVATE (now public but accessible thru routine)
  !=========================================================
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impSmats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impSreal
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impGmats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impGreal
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impG0mats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impG0real
  !
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impSAmats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impSAreal
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impFmats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impFreal
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impF0mats
  complex(8),allocatable,dimension(:,:,:,:,:)        :: impF0real
  !
  complex(8),allocatable,dimension(:)                :: impDmats_ph
  complex(8),allocatable,dimension(:)                :: impDreal_ph
  !
  type(GFmatrix),allocatable,dimension(:,:,:,:)      :: impGmatrix    
  type(GFmatrix)                                     :: impDmatrix



  !Spin Susceptibilities
  !=========================================================
  real(8),allocatable,dimension(:,:,:)               :: spinChi_tau
  complex(8),allocatable,dimension(:,:,:)            :: spinChi_w
  complex(8),allocatable,dimension(:,:,:)            :: spinChi_iv
  !
  type(GFmatrix),allocatable,dimension(:,:)          :: spinChimatrix

  !Diagonal/Off-diagonal charge-charge Susceptibilities
  !=========================================================  
  real(8),allocatable,dimension(:,:,:)               :: densChi_tau
  complex(8),allocatable,dimension(:,:,:)            :: densChi_w
  complex(8),allocatable,dimension(:,:,:)            :: densChi_iv
  !
  type(GFmatrix),allocatable,dimension(:,:)          :: densChimatrix


  !Pair-Pair Susceptibilities
  !=========================================================
  real(8),allocatable,dimension(:,:,:)               :: pairChi_tau
  complex(8),allocatable,dimension(:,:,:)            :: pairChi_w
  complex(8),allocatable,dimension(:,:,:)            :: pairChi_iv
  !
  type(GFmatrix),allocatable,dimension(:,:)          :: pairChimatrix



  !Exciton Susceptibilities
  !=========================================================
  real(8),allocatable,dimension(:,:,:,:)             :: exctChi_tau ![0:4,:]
  complex(8),allocatable,dimension(:,:,:,:)          :: exctChi_w
  complex(8),allocatable,dimension(:,:,:,:)          :: exctChi_iv
  !
  type(GFmatrix),allocatable,dimension(:,:,:)        :: exctChimatrix



  !Density and double occupancy
  !Local energies and generalized double occupancies
  !PRIVATE (now public but accessible thru routines)
  !=========================================================
  real(8),dimension(:),allocatable                   :: ed_dens
  real(8),dimension(:),allocatable                   :: ed_dens_up,ed_dens_dw
  real(8),dimension(:),allocatable                   :: ed_docc
  real(8),dimension(:),allocatable                   :: ed_phisc
  real(8),dimension(:,:),allocatable                 :: ed_mag
  real(8)                                            :: ed_Ekin
  real(8)                                            :: ed_Epot
  real(8)                                            :: ed_Eint
  real(8)                                            :: ed_Ehartree
  real(8)                                            :: ed_Eknot
  real(8)                                            :: ed_Eeph
  real(8)                                            :: ed_Dust,ed_Dund,ed_Dse,ed_Dph


  !Frequency and time arrays:
  !=========================================================
  real(8),dimension(:),allocatable                   :: wm,tau,wr,vm,vr


  !Impurity operators
  !PRIVATE (now public but accessible thru routine)
  !=========================================================
  complex(8),allocatable,dimension(:,:,:,:)          :: imp_density_matrix
  integer,parameter,dimension(3)                     :: Lzdiag = [-1,+1,0]
  integer,parameter,dimension(2)                     :: Szdiag = [1,-1]
  real(8),dimension(:,:),allocatable                 :: spin_field ![Norb,3=x,y,z]


  !--------------- LATTICE WRAP VARIABLES -----------------!
  complex(8),dimension(:,:,:,:,:),allocatable        :: Hloc_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: Smats_ineq,Sreal_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: SAmats_ineq,SAreal_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: Gmats_ineq,Greal_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: Fmats_ineq,Freal_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: G0mats_ineq,G0real_ineq
  complex(8),dimension(:,:,:,:,:,:),allocatable,save :: F0mats_ineq,F0real_ineq
  complex(8),dimension(:,:),allocatable,save         :: Dmats_ph_ineq,Dreal_ph_ineq
  complex(8),dimension(:,:,:,:,:),allocatable,save   :: imp_density_matrix_ineq
  real(8),dimension(:,:),allocatable,save            :: dens_ineq 
  real(8),dimension(:,:),allocatable,save            :: docc_ineq
  real(8),dimension(:,:,:),allocatable,save          :: mag_ineq
  real(8),dimension(:,:),allocatable,save            :: phisc_ineq
  real(8),dimension(:,:),allocatable,save            :: dd_ineq,e_ineq
  integer,allocatable,dimension(:,:)                 :: neigen_sector_ineq
  integer,allocatable,dimension(:)                   :: neigen_total_ineq
  real(8),dimension(:,:,:),allocatable               :: Hreplica_lambda_ineq ![Nineq,Nbath,Nsym]
  real(8),dimension(:,:,:),allocatable               :: Hgeneral_lambda_ineq ![Nineq,Nbath,Nsym]


  
  !File suffixes for printing fine tuning.
  !=========================================================
  character(len=32)                                  :: ed_file_suffix=""       !suffix string attached to the output files.
  character(len=10)                                  :: ineq_site_suffix="_ineq"
  integer                                            :: site_indx_padding=4
  logical                                            :: Jhflag              !spin-exchange and pair-hopping flag.
  logical                                            :: offdiag_gf_flag=.false.
  ! character(len=200)                               :: ed_input_file=""


  !This is the internal Mpi Communicator and variables.
  !=========================================================
#ifdef _MPI
  integer                                            :: MpiComm_Global=MPI_COMM_NULL
  integer                                            :: MpiComm=MPI_COMM_NULL
#endif
  integer                                            :: MpiGroup_Global=MPI_GROUP_NULL
  integer                                            :: MpiGroup=MPI_GROUP_NULL
  logical                                            :: MpiStatus=.false.
  logical                                            :: MpiMaster=.true.
  integer                                            :: MpiRank=0
  integer                                            :: MpiSize=1
  integer,allocatable,dimension(:)                   :: MpiMembers
  integer                                            :: mpiQup=0
  integer                                            :: mpiRup=0
  integer                                            :: mpiQdw=0
  integer                                            :: mpiRdw=0
  integer                                            :: mpiQ=0
  integer                                            :: mpiR=0
  integer                                            :: mpiIstart
  integer                                            :: mpiIend
  integer                                            :: mpiIshift
  logical                                            :: mpiAllThreads=.true.
  !

contains






  !=========================================================
  subroutine ed_set_MpiComm()
#ifdef _MPI
    integer :: ierr
    ! call MPI_Comm_dup(Comm,MpiComm_Global,ierr)
    ! call MPI_Comm_dup(Comm,MpiComm,ierr)
    MpiComm_Global = MPI_COMM_WORLD
    MpiComm        = MPI_COMM_WORLD
    call Mpi_Comm_group(MpiComm_Global,MpiGroup_Global,ierr)
    MpiStatus      = .true.
    MpiSize        = get_Size_MPI(MpiComm_Global)
    MpiRank        = get_Rank_MPI(MpiComm_Global)
    MpiMaster      = get_Master_MPI(MpiComm_Global)
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG ed_set_MpiComm: setting MPI comm"
#endif
#endif
  end subroutine ed_set_MpiComm

  subroutine ed_del_MpiComm()
#ifdef _MPI    
    MpiComm_Global = MPI_UNDEFINED
    MpiComm        = MPI_UNDEFINED
    MpiGroup_Global= MPI_GROUP_NULL
    MpiStatus      = .false.
    MpiSize        = 1
    MpiRank        = 0
    MpiMaster      = .true.
#ifdef _DEBUG
    write(Logfile,"(A)")"DEBUG ed_del_MpiComm: deleting MPI comm"
#endif
#endif
  end subroutine ed_del_MpiComm
  !=========================================================




  !=========================================================
  !Allocate the channels in GFmatrix structure
  subroutine allocate_gfmatrix_Nstate(self,Nstate)
    type(GFmatrix) :: self
    integer        :: Nstate
#ifdef _DEBUG
    if(ed_verbose>2)write(Logfile,"(A)")"DEBUG allocate_gfmatrix_Nstate: allocate self"
#endif
    if(allocated(self%state))deallocate(self%state)
    allocate(self%state(Nstate))
    self%status=.true.
  end subroutine allocate_gfmatrix_Nstate

  subroutine allocate_gfmatrix_Nchan(self,istate,Nchan)
    type(GFmatrix) :: self
    integer        :: istate,Nchan
#ifdef _DEBUG
    if(ed_verbose>3)write(Logfile,"(A)")"DEBUG allocate_gfmatrix_Nchan: allocate self, istate:"//str(istate)
#endif
    if(allocated(self%state(istate)%channel))deallocate(self%state(istate)%channel)
    allocate(self%state(istate)%channel(Nchan))

  end subroutine allocate_gfmatrix_Nchan

  !Allocate the Excitations spectrum at a given channel
  subroutine allocate_gfmatrix_Nexc(self,istate,ichan,Nexc)
    type(GFmatrix) :: self
    integer        :: istate,ichan
    integer        :: Nexc
#ifdef _DEBUG
    if(ed_verbose>4)write(Logfile,"(A,2I8)")"DEBUG allocate_gfmatrix_Nchan: allocate self, istate:"//str(istate)//", ichan:"//str(ichan)
#endif
    if(allocated(self%state(istate)%channel(ichan)%weight))&
         deallocate(self%state(istate)%channel(ichan)%weight)
    if(allocated(self%state(istate)%channel(ichan)%poles))&
         deallocate(self%state(istate)%channel(ichan)%poles)
    !
    allocate(self%state(istate)%channel(ichan)%weight(Nexc))
    allocate(self%state(istate)%channel(ichan)%poles(Nexc))
  end subroutine allocate_gfmatrix_Nexc





  subroutine deallocate_gfmatrix_single(self)
    type(GFmatrix) :: self
    integer        :: istate,ichan
#ifdef _DEBUG
    if(ed_verbose>3)write(Logfile,"(A)")"DEBUG deallocate_gfmatrix_single: deallocate self"
#endif
    if(self%status)then
       do istate=1,size(self%state)
          if(allocated(self%state(istate)%channel))then
             do ichan=1,size(self%state(istate)%channel)
                if(allocated(self%state(istate)%channel(ichan)%weight))&
                     deallocate(self%state(istate)%channel(ichan)%weight)
                !
                if(allocated(self%state(istate)%channel(ichan)%poles))&
                     deallocate(self%state(istate)%channel(ichan)%poles)
             enddo
             deallocate(self%state(istate)%channel)
          endif
       enddo
       deallocate(self%state)       
    endif
    self%status=.false.
  end subroutine deallocate_gfmatrix_single

  subroutine deallocate_gfmatrix_all1(self)
    type(GFmatrix),dimension(:) :: self
    integer                     :: i1
    do i1=1,size(self)
       call deallocate_gfmatrix_single(self(i1))
    enddo
  end subroutine deallocate_gfmatrix_all1

  subroutine deallocate_gfmatrix_all2(self)
    type(GFmatrix),dimension(:,:) :: self
    integer                       :: i1,i2
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          call deallocate_gfmatrix_single(self(i1,i2))
       enddo
    enddo
  end subroutine deallocate_gfmatrix_all2

  subroutine deallocate_gfmatrix_all3(self)
    type(GFmatrix),dimension(:,:,:) :: self
    integer                         :: i1,i2,i3
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             call deallocate_gfmatrix_single(self(i1,i2,i3))
          enddo
       enddo
    enddo
  end subroutine deallocate_gfmatrix_all3

  subroutine deallocate_gfmatrix_all4(self)
    type(GFmatrix),dimension(:,:,:,:) :: self
    integer                           :: i1,i2,i3,i4
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             do i4=1,size(self,4)
                call deallocate_gfmatrix_single(self(i1,i2,i3,i4))
             enddo
          enddo
       enddo
    enddo
  end subroutine deallocate_gfmatrix_all4



  !+-------------------------------------------------------------------+
  !PURPOSE  : WRITE GFmatrix to file
  !+-------------------------------------------------------------------+
  subroutine write_gfmatrix_single(self,file)
    class(GFmatrix)    :: self
    character(len=*)   :: file
    integer            :: unit_
#ifdef _DEBUG
    if(ed_verbose>3)write(Logfile,"(A)")"DEBUG write_gfmatrix_single: write self"
#endif
    unit_=free_unit()
    open(unit_,file=str(file))
    call write_formatted_gfmatrix(self,unit_)
    close(unit_)
  end subroutine write_gfmatrix_single

  subroutine write_gfmatrix_all1(self,file)
    class(GFmatrix)  :: self(:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1
    unit_=free_unit()
    open(unit_,file=str(file))
    do i1=1,size(self)
       call write_formatted_gfmatrix(self(i1),unit_)
    enddo
    close(unit_)
  end subroutine write_gfmatrix_all1

  subroutine write_gfmatrix_all2(self,file)
    class(GFmatrix)  :: self(:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2
    unit_=free_unit()
    open(unit_,file=str(file))
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          call write_formatted_gfmatrix(self(i1,i2),unit_)
       enddo
    enddo
    close(unit_)
  end subroutine write_gfmatrix_all2

  subroutine write_gfmatrix_all3(self,file)
    class(GFmatrix)  :: self(:,:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2,i3
    unit_=free_unit()
    open(unit_,file=str(file))
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             call write_formatted_gfmatrix(self(i1,i2,i3),unit_)
          enddo
       enddo
    enddo
    close(unit_)
  end subroutine write_gfmatrix_all3

  subroutine write_gfmatrix_all4(self,file)
    class(GFmatrix)  :: self(:,:,:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2,i3,i4
    unit_=free_unit()
    open(unit_,file=str(file))
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             do i4=1,size(self,4)
                call write_formatted_gfmatrix(self(i1,i2,i3,i4),unit_)
             enddo
          enddo
       enddo
    enddo
    close(unit_)
  end subroutine write_gfmatrix_all4




  !+-------------------------------------------------------------------+
  !PURPOSE  : Read cluster GF from file
  !+-------------------------------------------------------------------+
  subroutine read_gfmatrix_single(self,file)
    class(GFmatrix)  :: self
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2,i3,i4
#ifdef _DEBUG
    if(ed_verbose>4)write(Logfile,"(A)")"DEBUG reading_gfmatrix_single: reading self"
#endif
    call deallocate_GFmatrix(self)
    unit_=free_unit()
    open(unit_,file=str(file))
    rewind(unit_)
    call read_formatted_gfmatrix(self,unit_)
    close(unit_)
  end subroutine read_gfmatrix_single

  subroutine read_gfmatrix_all1(self,file)
    class(GFmatrix)  :: self(:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1
    call deallocate_GFmatrix(self)
    unit_=free_unit()
    open(unit_,file=str(file))
    rewind(unit_)
    do i1=1,size(self)
       call read_formatted_gfmatrix(self(i1),unit_)
    enddo
    close(unit_)
  end subroutine read_gfmatrix_all1

  subroutine read_gfmatrix_all2(self,file)
    class(GFmatrix)  :: self(:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2
    call deallocate_GFmatrix(self)
    unit_=free_unit()
    open(unit_,file=str(file))
    rewind(unit_)
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          call read_formatted_gfmatrix(self(i1,i2),unit_)
       enddo
    enddo
    close(unit_)
  end subroutine read_gfmatrix_all2

  subroutine read_gfmatrix_all3(self,file)
    class(GFmatrix)  :: self(:,:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2,i3
    call deallocate_GFmatrix(self)
    unit_=free_unit()
    open(unit_,file=str(file))
    rewind(unit_)
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             call read_formatted_gfmatrix(self(i1,i2,i3),unit_)
          enddo
       enddo
    enddo
    close(unit_)
  end subroutine read_gfmatrix_all3

  subroutine read_gfmatrix_all4(self,file)
    class(GFmatrix)  :: self(:,:,:,:)
    character(len=*) :: file
    integer          :: unit_
    integer          :: i1,i2,i3,i4
    call deallocate_GFmatrix(self)
    unit_=free_unit()
    open(unit_,file=str(file))
    rewind(unit_)
    do i1=1,size(self,1)
       do i2=1,size(self,2)
          do i3=1,size(self,3)
             do i4=1,size(self,4)
                call read_formatted_gfmatrix(self(i1,i2,i3,i4),unit_)
             enddo
          enddo
       enddo
    enddo
    close(unit_)
  end subroutine read_gfmatrix_all4



  !+-------------------------------------------------------------------+
  !PURPOSE  : write overload for GFmatrix type (formatted)
  !+-------------------------------------------------------------------+
  subroutine write_formatted_gfmatrix(dtv, unit)
    class(GFmatrix), intent(in)         :: dtv
    integer, intent(in)                 :: unit
    integer                             :: iexc,Ichan,istate
    integer                             :: Nexc,Nchan,Nstates
    write(unit,*) dtv%status
    if(.not.dtv%status)return
    Nstates = size(dtv%state)
    write(unit,*) Nstates
    do istate=1,Nstates
       Nchan = size(dtv%state(istate)%channel)
       write(unit,*)Nchan
       do ichan=1,Nchan
          write(unit,*) size(dtv%state(istate)%channel(ichan)%poles)
          write(unit,*) dtv%state(istate)%channel(ichan)%poles
          write(unit,*) dtv%state(istate)%channel(ichan)%weight
       enddo
    enddo
    write(unit,*)""
  end subroutine write_formatted_gfmatrix

  !+-------------------------------------------------------------------+
  !PURPOSE  : read overload for GFmatrix type (formatted)
  !+-------------------------------------------------------------------+
  subroutine read_formatted_gfmatrix(dtv, unit)
    class(GFmatrix), intent(inout)                :: dtv
    integer, intent(in)                           :: unit
    logical                                       :: alloc
    integer                                       :: ichan,Nchan,Nlanc,istate,Nstates
    !
    read(unit,*) alloc
    if(.not.alloc)return
    read(unit,*)Nstates
    call allocate_GFmatrix(dtv,Nstate=Nstates)
    do istate=1,Nstates
       read(unit,*)Nchan
       call allocate_GFmatrix(dtv,istate=istate,Nchan=Nchan)
       do ichan=1,Nchan
          read(unit,*)Nlanc
          call allocate_GFmatrix(dtv,istate=istate,ichan=ichan,Nexc=Nlanc)
          read(unit,*) dtv%state(istate)%channel(ichan)%poles
          read(unit,*) dtv%state(istate)%channel(ichan)%weight
       enddo
    enddo
    !
  end subroutine read_formatted_gfmatrix


END MODULE ED_VARS_GLOBAL
