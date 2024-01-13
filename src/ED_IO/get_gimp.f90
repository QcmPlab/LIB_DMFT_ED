subroutine ed_get_gimp_site_n3(self,axis,type)
  complex(8),dimension(:,:,:),intent(inout) :: self
  character(len=*),optional                 :: axis
  character(len=*),optional                 :: type
  character(len=1)                          :: axis_
  character(len=1)                          :: type_
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  select case(type_)
  case default; stop "ed_get_gimp ERROR: type is neither Normal, nor Anomalous"
  case ('n','N')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nspin*Norb,Nspin*Norb,Lmats],'ed_get_gimp','self')
        self = nn2so_reshape(impGmats,Nspin,Norb,Lmats)
     case('r','R')
        call assert_shape(self,[Nspin*Norb,Nspin*Norb,Lreal],'ed_get_gimp','self')
        self = nn2so_reshape(impGreal,Nspin,Norb,Lmats)
     end select
  case('a','A')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nspin*Norb,Nspin*Norb,Lmats],'ed_get_gimp','self')
        self = nn2so_reshape(impFmats,Nspin,Norb,Lmats)
     case('r','R')
        call assert_shape(self,[Nspin*Norb,Nspin*Norb,Lreal],'ed_get_gimp','self')
        self = nn2so_reshape(impFreal,Nspin,Norb,Lmats)
     end select
  end select
end subroutine ed_get_gimp_site_n3


subroutine ed_get_gimp_site_n5(self,axis,type)
  complex(8),dimension(:,:,:,:,:),intent(inout) :: self
  character(len=*),optional                     :: axis
  character(len=*),optional                     :: type
  character(len=1)                              :: axis_
  character(len=1)                              :: type_
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  select case(type_)
  case default; stop "ed_get_gimp ERROR: type is neither Normal, nor Anomalous"
  case ('n','N')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nspin,Nspin,Norb,Norb,Lmats],'ed_get_gimp','self')
        self = impGmats
     case('r','R')
        call assert_shape(self,[Nspin,Nspin,Norb,Norb,Lreal],'ed_get_gimp','self')
        self = impGreal
     end select
  case('a','A')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nspin,Nspin,Norb,Norb,Lmats],'ed_get_gimp','self')
        self = impFmats
     case('r','R')
        call assert_shape(self,[Nspin,Nspin,Norb,Norb,Lreal],'ed_get_gimp','self')
        self = impFreal
     end select
  end select
end subroutine ed_get_gimp_site_n5


!##################################################################
!##################################################################
!##################################################################


subroutine ed_get_gimp_lattice_n3(self,nlat,axis,type)
  complex(8),dimension(:,:,:),intent(inout) :: self
  integer,intent(in)                     :: nlat
  character(len=*),optional              :: axis
  character(len=*),optional              :: type
  character(len=1)                       :: axis_
  character(len=1)                       :: type_
  integer                                :: ilat
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  if(Nlat/=size(Gmats_ineq,1))stop "ERROR ed_get_gimp: wrong Nlat"
  select case(type_)
  case default; stop "ed_get_gimp ERROR: type is neither Normal, nor Anomalous"
  case ('n','N')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat*Nspin*Norb,Nlat*Nspin*Norb,Lmats],'ed_get_gimp','self')
        self = nnn2lso_reshape(Gmats_ineq,Nlat,Nspin,Norb,Lmats)
     case('r','R')
        call assert_shape(self,[Nlat*Nspin*Norb,Nlat*Nspin*Norb,Lreal],'ed_get_gimp','self')
        self = nnn2lso_reshape(Freal_ineq,Nlat,Nspin,Norb,Lreal)
     end select
  case('a','A')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat*Nspin*Norb,Nlat*Nspin*Norb,Lmats],'ed_get_gimp','self')
        self = nnn2lso_reshape(Fmats_ineq,Nlat,Nspin,Norb,Lmats)
     case('r','R')
        call assert_shape(self,[Nlat*Nspin*Norb,Nlat*Nspin*Norb,Lreal],'ed_get_gimp','self')
        self = nnn2lso_reshape(Freal_ineq,Nlat,Nspin,Norb,Lreal)
     end select
  end select
end subroutine ed_get_gimp_lattice_n3

subroutine ed_get_gimp_lattice_n4(self,nlat,axis,type)
  complex(8),dimension(:,:,:,:),intent(inout) :: self
  integer,intent(in)                     :: nlat
  character(len=*),optional              :: axis
  character(len=*),optional              :: type
  character(len=1)                       :: axis_
  character(len=1)                       :: type_
  integer                                :: ilat
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  if(Nlat/=size(Gmats_ineq,1))stop "ERROR ed_get_gimp: wrong Nlat"
  select case(type_)
  case default; stop "ed_get_gimp ERROR: type is neither Normal, nor Anomalous"
  case ('n','N')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat,Nspin*Norb,Nspin*Norb,Lmats],'ed_get_gimp','self')
        do ilat=1,Nlat
           self(ilat,:,:,:) = nn2so_reshape(Gmats_ineq(ilat,:,:,:,:,:),Nspin,Norb,Lmats)
        enddo
     case('r','R')
        call assert_shape(self,[Nlat,Nspin*Norb,Nspin*Norb,Lreal],'ed_get_gimp','self')
        do ilat=1,Nlat
           self(ilat,:,:,:) = nn2so_reshape(Freal_ineq(ilat,:,:,:,:,:),Nspin,Norb,Lreal)
        enddo
     end select
  case('a','A')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat,Nspin*Norb,Nspin*Norb,Lmats],'ed_get_gimp','self')
        do ilat=1,Nlat
           self(ilat,:,:,:) = nn2so_reshape(Fmats_ineq(ilat,:,:,:,:,:),Nspin,Norb,Lmats)
        enddo
     case('r','R')
        call assert_shape(self,[Nlat,Nspin*Norb,Nspin*Norb,Lreal],'ed_get_gimp','self')
        do ilat=1,Nlat
           self(ilat,:,:,:) = nn2so_reshape(Freal_ineq(ilat,:,:,:,:,:),Nspin,Norb,Lreal)
        enddo
     end select
  end select
end subroutine ed_get_gimp_lattice_n4

subroutine ed_get_gimp_lattice_n6(self,nlat,axis,type)
  complex(8),dimension(:,:,:,:,:,:),intent(inout) :: self
  integer,intent(in)                              :: nlat
  character(len=*),optional                       :: axis
  character(len=*),optional                       :: type
  character(len=1)                                :: axis_
  character(len=1)                                :: type_
  integer                                         :: ilat
  axis_='m';if(present(axis))axis_=trim(axis)
  type_='n';if(present(type))type_=trim(type)
  if(Nlat/=size(Gmats_ineq,1))stop "ERROR ed_get_gimp: wrong Nlat"
  select case(type_)
  case default; stop "ed_get_gimp ERROR: type is neither Normal, nor Anomalous"
  case ('n','N')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat,Nspin,Nspin,Norb,Norb,Lmats],'ed_get_gimp','self')
        self = Gmats_ineq
     case('r','R')
        call assert_shape(self,[Nlat,Nspin,Nspin,Norb,Norb,Lreal],'ed_get_gimp','self')
        self = Freal_ineq
     end select
  case('a','A')
     select case(axis_)
     case default;stop "ed_get_gimp ERROR: axis is neither Matsubara, nor Realaxis"
     case ('m','M')
        call assert_shape(self,[Nlat,Nspin,Nspin,Norb,Norb,Lmats],'ed_get_gimp','self')
        self = Fmats_ineq
     case('r','R')
        call assert_shape(self,[Nlat,Nspin,Nspin,Norb,Norb,Lreal],'ed_get_gimp','self')
        self = Freal_ineq
     end select
  end select
end subroutine ed_get_gimp_lattice_n6



