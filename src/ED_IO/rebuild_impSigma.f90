subroutine rebuild_sigma_single_n3(zeta,sigma,self)
  complex(8),dimension(:)                     :: zeta
  complex(8),dimension(:,:,:)                    :: Sigma
  complex(8),dimension(:,:,:),optional           :: Self
  integer                                     :: i,L
  logical                                     :: check
  complex(8),dimension(:,:,:,:,:),allocatable :: sn,sa
  !
  call ed_read_impGmatrix()
  !
  if(.not.allocated(impHloc))stop "rebuild_sigma error: impHloc not allocated. Call ed_set_Hloc first."  
  !
  call allocate_dmft_bath(dmft_bath)
  call init_dmft_bath(dmft_bath,used=.true.)
  !
  L = size(zeta)
  allocate(sn(Nspin,Nspin,Norb,Norb,L))
  allocate(sa(Nspin,Nspin,Norb,Norb,L))
  select case(ed_mode)
  case default;
     call rebuild_sigma_normal(zeta,sn)
  case("superc");
     if(.not.present(self))stop "rebuild_sigma ERROR: self not present in ed_mode=superc"
     call rebuild_sigma_superc(zeta,sn,sa)
  case("nonsu2");
     call rebuild_sigma_nonsu2(zeta,sn)
  end select
  !
  call deallocate_dmft_bath(dmft_bath)
  call deallocate_GFmatrix(impGmatrix)
  !
  call assert_shape(sigma,[Nspin*Norb,Nspin*Norb,L],'rebuild_sigma','sigma')
  sigma = nn2so_reshape(sn,Nspin,Norb,L)
  if(present(self))then
     call assert_shape(self,[Nspin*Norb,Nspin*Norb,L],'rebuild_sigma','self')
     self = nn2so_reshape(sa,Nspin,Norb,L)
  endif
  return
end subroutine rebuild_sigma_single_n3

subroutine rebuild_sigma_single_n5(zeta,sigma,self)
  complex(8),dimension(:)                     :: zeta
  complex(8),dimension(:,:,:,:,:)                    :: Sigma
  complex(8),dimension(:,:,:,:,:),optional           :: Self
  integer                                     :: i,L
  logical                                     :: check
  complex(8),dimension(:,:,:,:,:),allocatable :: sn,sa
  !
  call ed_read_impGmatrix()
  !
  if(.not.allocated(impHloc))stop "rebuild_sigma error: impHloc not allocated. Call ed_set_Hloc first."  
  !
  call allocate_dmft_bath(dmft_bath)
  call init_dmft_bath(dmft_bath,used=.true.)
  !
  L = size(zeta)
  allocate(sn(Nspin,Nspin,Norb,Norb,L))
  allocate(sa(Nspin,Nspin,Norb,Norb,L))
  select case(ed_mode)
  case default;
     call rebuild_sigma_normal(zeta,sn)
  case("superc");
     if(.not.present(self))stop "rebuild_sigma ERROR: self not present in ed_mode=superc"
     call rebuild_sigma_superc(zeta,sn,sa)
  case("nonsu2");
     call rebuild_sigma_nonsu2(zeta,sn)
  end select
  !
  call deallocate_dmft_bath(dmft_bath)
  call deallocate_GFmatrix(impGmatrix)
  !
  call assert_shape(sigma,[Nspin,Nspin,Norb,Norb,L],'rebuild_sigma','sigma')
  sigma = sn
  if(present(self))then
     call assert_shape(self,[Nspin,Nspin,Norb,Norb,L],'rebuild_sigma','self')
     self = sa
  endif
  return
end subroutine rebuild_sigma_single_n5



!##################################################################
!##################################################################
!##################################################################


subroutine rebuild_sigma_ineq_n3(zeta,Nlat,sigma,self)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:)                      :: Sigma
  complex(8),dimension(:,:,:),optional             :: Self
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: sn,sa
  !
  Nineq=Nlat
  L=size(zeta)
  !
  if(.not.allocated(Hloc_ineq))stop "ed_rebuild_gf error: Hloc_ineq not allocated. Call ed_set_Hloc first."
  !
  allocate(Sn(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Sa(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     call ed_set_Hloc(Hloc_ineq(ilat,:,:,:,:))
     if(present(self))then
        call rebuild_sigma_single_n5(zeta,sn(ilat,:,:,:,:,:),sa(ilat,:,:,:,:,:))
     else
        call rebuild_sigma_single_n5(zeta,sa(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(sigma,[Nineq*Nspin*Norb,Nineq*Nspin*Norb,L],'rebuild_sigma','sigma')
  sigma = nnn2lso_reshape(sn,Nineq,Nspin,Norb,L)
  if(present(self))then
     call assert_shape(self,[Nineq*Nspin*Norb,Nineq*Nspin*Norb,L],'rebuild_sigma','self')
     self = nnn2lso_reshape(sa,Nineq,Nspin,Norb,L)
  endif
  return
end subroutine rebuild_sigma_ineq_n3

subroutine rebuild_sigma_ineq_n4(zeta,Nlat,sigma,self)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:,:)                      :: Sigma
  complex(8),dimension(:,:,:,:),optional             :: Self
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: sn,sa
  !
  Nineq=Nlat
  L=size(zeta)
  !
  if(.not.allocated(Hloc_ineq))stop "ed_rebuild_gf error: Hloc_ineq not allocated. Call ed_set_Hloc first."
  !
  allocate(Sn(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Sa(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     call ed_set_Hloc(Hloc_ineq(ilat,:,:,:,:))
     if(present(self))then
        call rebuild_sigma_single_n5(zeta,sn(ilat,:,:,:,:,:),sa(ilat,:,:,:,:,:))
     else
        call rebuild_sigma_single_n5(zeta,sa(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(sigma,[Nineq,Nspin*Norb,Nspin*Norb,L],'rebuild_sigma','sigma')
  do ilat=1,Nineq
     sigma(ilat,:,:,:) = nn2so_reshape(sn(ilat,:,:,:,:,:),Nspin,Norb,L)
  enddo
  if(present(self))then
     call assert_shape(self,[Nineq,Nspin*Norb,Nspin*Norb,L],'rebuild_sigma','self')
     do ilat=1,Nineq
        self(ilat,:,:,:) = nn2so_reshape(sa(ilat,:,:,:,:,:),Nspin,Norb,L)
     enddo
  endif
  return
end subroutine rebuild_sigma_ineq_n4


subroutine rebuild_sigma_ineq_n6(zeta,Nlat,sigma,self)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:,:,:,:)                      :: Sigma
  complex(8),dimension(:,:,:,:,:,:),optional             :: Self
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: sn,sa
  !
  Nineq=Nlat
  L=size(zeta)
  !
  if(.not.allocated(Hloc_ineq))stop "ed_rebuild_gf error: Hloc_ineq not allocated. Call ed_set_Hloc first."
  !
  allocate(Sn(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Sa(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     call ed_set_Hloc(Hloc_ineq(ilat,:,:,:,:))
     if(present(self))then
        call rebuild_sigma_single_n5(zeta,sn(ilat,:,:,:,:,:),sa(ilat,:,:,:,:,:))
     else
        call rebuild_sigma_single_n5(zeta,sa(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(sigma,[Nineq,Nspin,Nspin,Norb,Norb,L],'rebuild_sigma','sigma')
  sigma = sn
  if(present(self))then
     call assert_shape(self,[Nineq,Nspin,Nspin,Norb,Norb,L],'rebuild_sigma','self')
     self = sa
  endif
  return
end subroutine rebuild_sigma_ineq_n6










subroutine rebuild_sigma_normal(zeta,sigma)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: sigma
  integer                                                :: ispin,jspin
  integer                                                :: i,iorb,jorb
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: G,invG0,invG
  complex(8),dimension(Norb,Norb)                        :: Gmat
  !
  Sigma = zero
  invG0 = zero
  invG  = zero
  !  
  !Get Gimp
  call rebuild_gimp_normal(zeta,G)
  !
  !Get G0^-1
  invG0   = invg0_bath_function(zeta,dmft_bath)
  !
  !Get Gimp^-1
  select case(bath_type)
  case default
     do ispin=1,Nspin
        do iorb=1,Norb
           invG(ispin,ispin,iorb,iorb,:)  = one/G(ispin,ispin,iorb,iorb,:)
           Sigma(ispin,ispin,iorb,iorb,:) = invG0(ispin,ispin,iorb,iorb,:) - invG(ispin,ispin,iorb,iorb,:)
        enddo
     enddo
     !
  case ("hybrid","replica","general")   !Diagonal in spin
     do ispin=1,Nspin
        do i=1,size(zeta)
           Gmat = G(ispin,ispin,:,:,i)
           call inv(Gmat)
           Sigma(ispin,ispin,:,:,i)= invG0(ispin,ispin,:,:,i) - Gmat
        enddo
     enddo
  end select
  return
end subroutine rebuild_sigma_normal



subroutine rebuild_sigma_nonsu2(zeta,sigma)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: sigma
  integer                                                :: ispin,jspin
  integer                                                :: i,iorb,jorb
  integer                                                :: io,jo
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: G,invG0,invG
  complex(8),dimension(Nspin*Norb,Nspin*Norb)            :: Gmat
  !
  Sigma = zero
  invG0 = zero
  invG  = zero
  !  
  !Get Gimp
  call rebuild_gimp_normal(zeta,G)
  !
  !Get G0^-1
  invG0 = invg0_bath_function(zeta,dmft_bath)
  !
  !Get Gimp^-1
  select case(bath_type)
  case ("normal")
     do i=1,size(zeta)
        do ispin=1,Nspin
           do jspin=1,Nspin
              do iorb=1,Norb
                 jorb= iorb
                 io  = iorb + (ispin-1)*Norb
                 jo  = jorb + (jspin-1)*Norb
                 Gmat(io,jo) = G(ispin,jspin,iorb,jorb,i)
              enddo
           enddo
        enddo
        call inv(Gmat)
        do ispin=1,Nspin
           do jspin=1,Nspin
              do iorb=1,Norb
                 jorb= iorb
                 io  = iorb + (ispin-1)*Norb
                 jo  = jorb + (jspin-1)*Norb
                 Sigma(ispin,jspin,iorb,jorb,i) = invG0(ispin,jspin,iorb,jorb,i) - Gmat(io,jo) 
              enddo
           enddo
        enddo
     enddo
     !
  case ("hybrid","replica","general")
     do i=1,size(zeta)
        Gmat  = nn2so_reshape(G(:,:,:,:,i),Nspin,Norb)
        call inv(Gmat)
        do ispin=1,Nspin
           do jspin=1,Nspin
              do iorb=1,Norb
                 do jorb=1,Norb
                    io = iorb + (ispin-1)*Norb
                    jo = jorb + (jspin-1)*Norb
                    Sigma(ispin,jspin,iorb,jorb,i) = invG0(ispin,jspin,iorb,jorb,i) - Gmat(io,jo)
                 enddo
              enddo
           enddo
        enddo
     enddo
     !
  end select
  !
  return
  !     
end subroutine rebuild_sigma_nonsu2



subroutine rebuild_sigma_superc(zeta,sigma,self)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: sigma,self
  integer                                                :: ispin,jspin
  integer                                                :: i,iorb,jorb
  integer                                                :: Lzeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: G,F
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: invG0,invF0
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: invG,invF
  complex(8),dimension(2*Nspin*Norb,2*Nspin*Norb)        :: Gmat
  real(8),dimension(size(zeta))                          :: det_mats
  complex(8),dimension(size(zeta))                       :: det_real
  logical                                                :: imZ
  !
  Sigma = zero
  Self  = zero
  invG0 = zero
  invF0 = zero  
  invG  = zero
  invF  = zero
  !
  Lzeta= size(zeta)
  ispin= 1
  imZ  = all( dreal(zeta(:))==0d0 )
  !
  !Get Gimp
  call rebuild_gimp_superc(zeta,G,F)
  !
  !
  !Get G0^-1,F0^-1: a little trick to use array with a single frequency
  if(imZ)then
     invG0 = invg0_bath_function(zeta,dmft_bath)
     invF0 = invf0_bath_function(zeta,dmft_bath)
  else
     invG0 = invg0_bath_function(zeta,dmft_bath,axis='real')
     invF0 = invf0_bath_function(zeta,dmft_bath,axis='real')
  endif
  !
  !Get Gimp^-1
  select case(bath_type)
  case default
     do iorb=1,Norb
        if(imZ)then
           det_mats  =  abs(G(ispin,ispin,iorb,iorb,:))**2 + (F(ispin,ispin,iorb,iorb,:))**2
           invG(ispin,ispin,iorb,iorb,:) =  conjg(G(ispin,ispin,iorb,iorb,:))/det_mats
           invF(ispin,ispin,iorb,iorb,:) =        F(ispin,ispin,iorb,iorb,:)/det_mats
        else
           det_real  = -G(ispin,ispin,iorb,iorb,:)*conjg(G(ispin,ispin,iorb,iorb,Lzeta:1:-1)) - F(ispin,ispin,iorb,iorb,:)**2
           invG(ispin,ispin,iorb,iorb,:) = -conjg(G(ispin,ispin,iorb,iorb,Lzeta:1:-1))/det_real
           invF(ispin,ispin,iorb,iorb,:) =       -F(ispin,ispin,iorb,iorb,:)/det_real
        endif
        Sigma(ispin,ispin,iorb,iorb,:)  = invG0(ispin,ispin,iorb,iorb,:) - invG(ispin,ispin,iorb,iorb,:)
        Self(ispin,ispin,iorb,iorb,:)   = invF0(ispin,ispin,iorb,iorb,:) - invF(ispin,ispin,iorb,iorb,:)
     enddo
     !
     !
  case ("hybrid")
     do i=1,Lzeta
        if(imZ)then
           Gmat=zero
           Gmat(1:Norb,1:Norb)               = G(ispin,ispin,:,:,i)
           Gmat(1:Norb,Norb+1:2*Norb)        = F(ispin,ispin,:,:,i)
           Gmat(Norb+1:2*Norb,1:Norb)        = F(ispin,ispin,:,:,i)
           Gmat(Norb+1:2*Norb,Norb+1:2*Norb) =-conjg(G(ispin,ispin,:,:,i))
           call inv(Gmat)
           invG(ispin,ispin,:,:,i) = Gmat(1:Norb,1:Norb)
           invF(ispin,ispin,:,:,i) = Gmat(1:Norb,Norb+1:2*Norb)
        else
           Gmat=zero
           Gmat(1:Norb,1:Norb)               = G(ispin,ispin,:,:,i)
           Gmat(1:Norb,Norb+1:2*Norb)        = F(ispin,ispin,:,:,i)
           Gmat(Norb+1:2*Norb,1:Norb)        = F(ispin,ispin,:,:,i)
           Gmat(Norb+1:2*Norb,Norb+1:2*Norb) =-conjg(G(ispin,ispin,:,:,Lzeta-i+1))
           call inv(Gmat)
           invG(ispin,ispin,:,:,i) =  Gmat(1:Norb,1:Norb)
           invF(ispin,ispin,:,:,i) =  Gmat(1:Norb,Norb+1:2*Norb)
        endif
     enddo
     Sigma(ispin,ispin,:,:,:)  = invG0(ispin,ispin,:,:,:) - invG(ispin,ispin,:,:,:)
     Self(ispin,ispin,:,:,:)   = invF0(ispin,ispin,:,:,:) - invF(ispin,ispin,:,:,:)
  end select
  !
  return
  !
end subroutine rebuild_sigma_superc
