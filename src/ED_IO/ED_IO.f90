MODULE ED_IO
  USE ED_INPUT_VARS
  USE ED_VARS_GLOBAL
  USE ED_AUX_FUNX
  USE ED_SETUP
  USE ED_BATH
  !
  USE SF_LINALG
  USE SF_SPIN
  USE SF_ARRAYS,  only: linspace,arange
  USE SF_IOTOOLS, only: str,reg,free_unit,splot,sread
  USE SF_MISC,    only: assert_shape
  implicit none
  private

  ! !Retrieve self-energy through routines:
  interface ed_get_sigma
     module procedure :: ed_get_sigma_site_n3
     module procedure :: ed_get_sigma_site_n5
     module procedure :: ed_get_sigma_lattice_n3
     module procedure :: ed_get_sigma_lattice_n4
     module procedure :: ed_get_sigma_lattice_n6
  end interface ed_get_sigma

  !Retrieve imp GF through routines.
  interface ed_get_gimp
     module procedure :: ed_get_gimp_site_n3
     module procedure :: ed_get_gimp_site_n5
     module procedure :: ed_get_gimp_lattice_n3
     module procedure :: ed_get_gimp_lattice_n4
     module procedure :: ed_get_gimp_lattice_n6
  end interface ed_get_gimp

  !Retrieve imp GF_0 (G0_and) through routines.
  interface ed_get_g0imp
     module procedure :: ed_get_g0imp_site_n3
     module procedure :: ed_get_g0imp_site_n5
     module procedure :: ed_get_g0imp_lattice_n3
     module procedure :: ed_get_g0imp_lattice_n4
     module procedure :: ed_get_g0imp_lattice_n6
  end interface ed_get_g0imp


  !Rebuild impurity Sigma  or GF from saved poles&weights
  interface ed_build_gimp
     module procedure :: rebuild_gimp_single_n3
     module procedure :: rebuild_gimp_single_n5
     module procedure :: rebuild_gimp_ineq_n3
     module procedure :: rebuild_gimp_ineq_n4
     module procedure :: rebuild_gimp_ineq_n6
  end interface ed_build_gimp

  interface ed_build_sigma
     module procedure :: rebuild_sigma_single_n3
     module procedure :: rebuild_sigma_single_n5
     module procedure :: rebuild_sigma_ineq_n3
     module procedure :: rebuild_sigma_ineq_n4
     module procedure :: rebuild_sigma_ineq_n6
  end interface ed_build_sigma


  !Build Gand/Delta from a user bath
  interface ed_get_g0and
     module procedure :: ed_get_g0and_n3
     module procedure :: ed_get_g0and_n5
  end interface ed_get_g0and

  interface ed_get_delta
     module procedure :: ed_get_delta_n3
     module procedure :: ed_get_delta_n5
  end interface ed_get_delta


  !Observables
  interface ed_get_dens
     module procedure :: ed_get_dens_n0
     module procedure :: ed_get_dens_n1
     module procedure :: ed_get_dens_n2
  end interface ed_get_dens

  interface ed_get_mag
     module procedure :: ed_get_mag_n0
     module procedure :: ed_get_mag_n1
     module procedure :: ed_get_mag_n2
     module procedure :: ed_get_mag_n3
  end interface ed_get_mag

  interface ed_get_docc
     module procedure :: ed_get_docc_n0
     module procedure :: ed_get_docc_n1
     module procedure :: ed_get_docc_n2
  end interface ed_get_docc

  interface ed_get_phi
     module procedure :: ed_get_phisc_n0
     module procedure :: ed_get_phisc_n1
     module procedure :: ed_get_phisc_n2
  end interface ed_get_phi


  !Get Energies
  interface ed_get_eimp
     module procedure :: ed_get_eimp_n1
     module procedure :: ed_get_eimp_n2
  end interface ed_get_eimp

  interface ed_get_epot
     module procedure :: ed_get_epot_n0
     module procedure :: ed_get_epot_n1
  end interface ed_get_epot

  interface ed_get_eint
     module procedure :: ed_get_eint_n0
     module procedure :: ed_get_eint_n1
  end interface ed_get_eint

  interface ed_get_ehartree
     module procedure :: ed_get_ehartree_n0
     module procedure :: ed_get_ehartree_n1
  end interface ed_get_ehartree

  interface ed_get_eknot
     module procedure :: ed_get_eknot_n0
     module procedure :: ed_get_eknot_n1
  end interface ed_get_eknot


  !Get Double occupancies
  interface ed_get_doubles
     module procedure :: ed_get_doubles_n1
     module procedure :: ed_get_doubles_n2
  end interface ed_get_doubles

  interface ed_get_dust
     module procedure :: ed_get_dust_n0
     module procedure :: ed_get_dust_n1
  end interface ed_get_dust

  interface ed_get_dund
     module procedure :: ed_get_dund_n0
     module procedure :: ed_get_dund_n1
  end interface ed_get_dund

  interface ed_get_dse
     module procedure :: ed_get_dse_n0
     module procedure :: ed_get_dse_n1
  end interface ed_get_dse

  interface ed_get_dph
     module procedure :: ed_get_dph_n0
     module procedure :: ed_get_dph_n1
  end interface ed_get_dph



  

  interface ed_get_density_matrix
     module procedure :: ed_get_density_matrix_single
     module procedure :: ed_get_density_matrix_lattice
  end interface ed_get_density_matrix

  interface ed_read_impSigma
     module procedure :: ed_read_impSigma_single
     module procedure :: ed_read_impSigma_lattice
  end interface ed_read_impSigma


  public :: ed_get_sigma
  public :: ed_get_gimp
  public :: ed_get_g0imp
  public :: ed_get_g0and
  public :: ed_get_delta

  public :: ed_build_gimp
  public :: ed_build_sigma

  public :: ed_get_dens
  public :: ed_get_mag
  public :: ed_get_docc
  public :: ed_get_phi
  public :: ed_get_eimp
  public :: ed_get_epot
  public :: ed_get_eint 
  public :: ed_get_ehartree
  public :: ed_get_eknot
  public :: ed_get_doubles
  public :: ed_get_dust
  public :: ed_get_dund
  public :: ed_get_dse
  public :: ed_get_dph
  public :: ed_get_neigen_total
  !  
  public :: ed_get_density_matrix
  public :: ed_get_quantum_SOC_operators_single
  public :: ed_get_quantum_SOC_operators_lattice


  !****************************************************************************************!
  !****************************************************************************************!

  public :: ed_print_impSigma
  public :: ed_print_impG
  public :: ed_print_impG0
  public :: ed_print_impD
  public :: ed_print_impChi
  public :: ed_print_impGmatrix


  public :: ed_read_impSigma
  public :: ed_read_impGmatrix


  !****************************************************************************************!
  !****************************************************************************************!


  character(len=64)                :: suffix





contains


  !+-----------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve measured values of the impurity self-energy 
  !+-----------------------------------------------------------------------------+!
  include "get_sigma.f90"


  !+-----------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve measured values of the impurity green's functions 
  !+-----------------------------------------------------------------------------+!
  include "get_gimp.f90"

  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve measured values of the impurity green's functions 
  !+--------------------------------------------------------------------------+!
  include "get_g0imp.f90"

  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve Anderson non-interacting green's functions 
  !+--------------------------------------------------------------------------+!
  include "get_gand_bath.f90"

  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Re-build the Impurity green's functions and self-energy at
  !          arbitrary complex zeta
  !+--------------------------------------------------------------------------+!
  include "rebuild_impG.f90"
  include "rebuild_impSigma.f90"


  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve impurity density matrices and SOC operators
  !+--------------------------------------------------------------------------+!
  include "get_imp_dm.f90"
  include "get_imp_SOC_op.f90"

  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Retrieve measured values of the local observables
  !+--------------------------------------------------------------------------+!
  include "get_dens.f90"
  include "get_mag.f90"
  include "get_docc.f90"
  include "get_phi.f90"
  include "get_energy.f90"
  include "get_doubles.f90"
  include "get_neigen.f90"






  !+------------------------------------------------------------------+
  !PURPOSE  : Print impurity Functions case INTERNAL USE
  ! - impSigma
  ! - impG
  ! - impG0
  ! NORMAL - SUPERConducting - NONSU2
  !+------------------------------------------------------------------+
  include "print_impSigma.f90"
  subroutine ed_print_impSigma
    call allocate_grids
    select case(ed_mode)
    case ("normal");call print_impSigma_normal
    case ("superc");call print_impSigma_superc
    case ("nonsu2");call print_impSigma_nonsu2
    case default;stop "ed_print_impSigma error: ed_mode not in the list"
    end select
    call deallocate_grids
  end subroutine ed_print_impSigma


  include "print_impG.f90"
  subroutine ed_print_impG
    call allocate_grids
    select case(ed_mode)
    case ("normal");call print_impG_normal
    case ("superc");call print_impG_superc
    case ("nonsu2");call print_impG_nonsu2
    case default;stop "ed_print_impG error: ed_mode not in the list"
    end select
    call deallocate_grids
  end subroutine ed_print_impG


  include "print_impG0.f90"
  subroutine ed_print_impG0
    call allocate_grids
    select case(ed_mode)
    case ("normal");call print_impG0_normal
    case ("superc");call print_impG0_superc
    case ("nonsu2");call print_impG0_nonsu2
    case default;stop "ed_print_impG0 error: ed_mode not in the list"
    end select
    call deallocate_grids
  end subroutine ed_print_impG0

  subroutine ed_print_impD
    call allocate_grids()
    !Print the impurity functions:
    call splot("impDph_iw.ed"   ,vm,impDmats_ph(:))
    call splot("impDph_realw.ed",vr,impDreal_ph(:))
    call deallocate_grids()
  end subroutine ed_print_impD


  include "print_impChi.f90"
  subroutine ed_print_impChi
    call allocate_grids
    if(chispin_flag)call print_chi_spin
    if(chidens_flag)call print_chi_dens
    if(chipair_flag)call print_chi_pair
    if(chiexct_flag)call print_chi_exct
    call deallocate_grids
  end subroutine ed_print_impChi


  subroutine ed_print_impGmatrix(file)
    character(len=*),optional :: file
    character(len=256)        :: file_
    if(.not.allocated(impGmatrix))stop "ED_PRINT_IMPGFMATRIX ERROR: impGmatrix not allocated!"
    file_="gfmatrix";if(present(file))file_=str(file)
    call write_GFmatrix(impGmatrix,str(file_)//str(ed_file_suffix)//".restart")
  end subroutine ed_print_impGmatrix




  !+--------------------------------------------------------------------------+!
  ! PURPOSE: Read self-energy function(s) - also for inequivalent sites.
  !+--------------------------------------------------------------------------+!
  include "read_impSigma.f90"
  subroutine ed_read_impSigma_single
    !
    if(allocated(impSmats))deallocate(impSmats)
    if(allocated(impSreal))deallocate(impSreal)
    if(allocated(impSAmats))deallocate(impSAmats)
    if(allocated(impSAreal))deallocate(impSAreal)
    allocate(impSmats(Nspin,Nspin,Norb,Norb,Lmats))
    allocate(impSreal(Nspin,Nspin,Norb,Norb,Lreal))
    allocate(impSAmats(Nspin,Nspin,Norb,Norb,Lmats)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    allocate(impSAreal(Nspin,Nspin,Norb,Norb,Lreal)) !THIS SHOULD NOT DEPEND ON SPIN: NSPIN=>1
    impSmats=zero
    impSreal=zero
    impSAmats=zero
    impSAreal=zero
    !
    select case(ed_mode)
    case ("normal");call read_impSigma_normal
    case ("superc");call read_impSigma_superc
    case ("nonsu2");call read_impSigma_nonsu2
    case default;stop "ed_read_impSigma error: ed_mode not in the list"
    end select
  end subroutine ed_read_impSigma_single

  subroutine ed_read_impSigma_lattice(Nineq)
    integer :: Nineq
    integer :: ilat
    !
    if(allocated(Smats_ineq))deallocate(Smats_ineq)
    if(allocated(Sreal_ineq))deallocate(Sreal_ineq)
    if(allocated(SAmats_ineq))deallocate(SAmats_ineq)
    if(allocated(SAreal_ineq))deallocate(SAreal_ineq)
    allocate(Smats_ineq(Nineq,Nspin,Nspin,Norb,Norb,Lmats))
    allocate(Sreal_ineq(Nineq,Nspin,Nspin,Norb,Norb,Lreal))
    allocate(SAmats_ineq(Nineq,Nspin,Nspin,Norb,Norb,Lmats))
    allocate(SAreal_ineq(Nineq,Nspin,Nspin,Norb,Norb,Lreal))
    Smats_ineq  = zero 
    Sreal_ineq  = zero 
    SAmats_ineq = zero 
    SAreal_ineq = zero
    !
    do ilat=1,Nineq
       ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
       call ed_read_impSigma_single
       Smats_ineq(ilat,:,:,:,:,:)  = impSmats
       Sreal_ineq(ilat,:,:,:,:,:)  = impSreal
       SAmats_ineq(ilat,:,:,:,:,:) = impSAmats
       SAreal_ineq(ilat,:,:,:,:,:) = impSAreal
    enddo
    ed_file_suffix=""
  end subroutine ed_read_impSigma_lattice


  !+-------------------------------------------------------------------+
  !PURPOSE  : Read cluster GF from file
  !+-------------------------------------------------------------------+
  subroutine ed_read_impGmatrix(file)
    character(len=*),optional :: file
    character(len=256)        :: file_
    !
    if(allocated(impGmatrix))call deallocate_GFmatrix(impGmatrix)
    if(allocated(impGmatrix))deallocate(impGmatrix)
    allocate(impGmatrix(Nspin,Nspin,Norb,Norb))
    file_="gfmatrix";if(present(file))file_=str(file)
    call read_GFmatrix(impGmatrix,str(file_)//str(ed_file_suffix)//".restart")
  end subroutine ed_read_impGmatrix


END MODULE ED_IO







