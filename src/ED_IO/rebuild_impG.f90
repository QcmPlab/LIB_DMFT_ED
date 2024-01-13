subroutine rebuild_gimp_single_n3(zeta,gimp,fimp)
  complex(8),dimension(:)                     :: zeta
  complex(8),dimension(:,:,:)                    :: Gimp
  complex(8),dimension(:,:,:),optional           :: Fimp
  integer                                     :: i,L
  logical                                     :: check
  complex(8),dimension(:,:,:,:,:),allocatable :: gf,ff
  !
  call ed_read_impGmatrix()
  !
  L = size(zeta)
  allocate(gf(Nspin,Nspin,Norb,Norb,L))
  allocate(ff(Nspin,Nspin,Norb,Norb,L))
  !
  select case(ed_mode)
  case default;
     call rebuild_gimp_normal(zeta,gf)
  case("superc");
     if(.not.present(fimp))stop "rebuild_gimp ERROR: fimp not present in ed_mode=superc"
     call rebuild_gimp_superc(zeta,gf,ff)
  case("nonsu2");
     call rebuild_gimp_nonsu2(zeta,gf)
  end select
  !
  call deallocate_GFmatrix(impGmatrix)
  !
  call assert_shape(gimp,[Nspin*Norb,Nspin*Norb,L],'rebuild_gimp','gimp')
  gimp = nn2so_reshape(gf,Nspin,Norb,L)
  if(present(fimp))then
     call assert_shape(fimp,[Nspin*Norb,Nspin*Norb,L],'rebuild_gimp','fimp')
     fimp = nn2so_reshape(ff,Nspin,Norb,L)
  endif
  return
end subroutine rebuild_gimp_single_n3

subroutine rebuild_gimp_single_n5(zeta,gimp,fimp)
  complex(8),dimension(:)                     :: zeta
  complex(8),dimension(:,:,:,:,:)                    :: Gimp
  complex(8),dimension(:,:,:,:,:),optional           :: Fimp
  integer                                     :: i,L
  logical                                     :: check
  complex(8),dimension(:,:,:,:,:),allocatable :: gf,ff
  !
  call ed_read_impGmatrix()
  !
  L = size(zeta)
  allocate(gf(Nspin,Nspin,Norb,Norb,L))
  allocate(ff(Nspin,Nspin,Norb,Norb,L))
  !
  select case(ed_mode)
  case default;
     call rebuild_gimp_normal(zeta,gf)
  case("superc");
     if(.not.present(fimp))stop "rebuild_gimp ERROR: fimp not present in ed_mode=superc"
     call rebuild_gimp_superc(zeta,gf,ff)
  case("nonsu2");
     call rebuild_gimp_nonsu2(zeta,gf)
  end select
  !
  call deallocate_GFmatrix(impGmatrix)
  !
  call assert_shape(gimp,[Nspin,Nspin,Norb,Norb,L],'rebuild_gimp','gimp')
  gimp = gf
  if(present(fimp))then
     call assert_shape(fimp,[Nspin,Nspin,Norb,Norb,L],'rebuild_gimp','fimp')
     fimp = ff
  endif
  return
end subroutine rebuild_gimp_single_n5


!##################################################################
!##################################################################
!##################################################################


subroutine rebuild_gimp_ineq_n3(zeta,Nlat,gimp,fimp)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:)                      :: Gimp
  complex(8),dimension(:,:,:),optional             :: Fimp
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: gf,ff
  !
  Nineq=Nlat
  L=size(zeta)
  !
  allocate(Gf(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Ff(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     if(present(fimp))then
        call rebuild_gimp_single_n5(zeta,&
             gf(ilat,:,:,:,:,:),&
             ff(ilat,:,:,:,:,:))
     else
        call rebuild_gimp_single_n5(zeta,&
             ff(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(gimp,[Nineq*Nspin*Norb,Nineq*Nspin*Norb,L],'rebuild_gimp','gimp')
  gimp = nnn2lso_reshape(gf,Nineq,Nspin,Norb,L)

  if(present(fimp))then
     call assert_shape(fimp,[Nineq*Nspin*Norb,Nineq*Nspin*Norb,L],'rebuild_gimp','fimp')
     fimp = nnn2lso_reshape(ff,Nineq,Nspin,Norb,L)
  endif
  return
end subroutine rebuild_gimp_ineq_n3

subroutine rebuild_gimp_ineq_n4(zeta,Nlat,gimp,fimp)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:,:)                      :: Gimp
  complex(8),dimension(:,:,:,:),optional             :: Fimp
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: gf,ff
  !
  Nineq=Nlat
  L=size(zeta)
  !
  allocate(Gf(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Ff(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     if(present(fimp))then
        call rebuild_gimp_single_n5(zeta,&
             gf(ilat,:,:,:,:,:),&
             ff(ilat,:,:,:,:,:))
     else
        call rebuild_gimp_single_n5(zeta,&
             ff(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(gimp,[Nineq,Nspin*Norb,Nspin*Norb,L],'rebuild_gimp','gimp')
  do ilat=1,Nineq
     gimp(ilat,:,:,:) = nn2so_reshape(gf(ilat,:,:,:,:,:),Nspin,Norb,L)
  enddo
  if(present(fimp))then
     call assert_shape(fimp,[Nineq,Nspin*Norb,Nspin*Norb,L],'rebuild_gimp','fimp')
     do ilat=1,Nineq
        fimp(ilat,:,:,:) = nn2so_reshape(ff(ilat,:,:,:,:,:),Nspin,Norb,L)
     enddo
  endif
  return
end subroutine rebuild_gimp_ineq_n4


subroutine rebuild_gimp_ineq_n6(zeta,Nlat,gimp,fimp)
  complex(8),dimension(:)                       :: zeta
  integer                                       :: Nlat
  complex(8),dimension(:,:,:,:,:,:)                      :: Gimp
  complex(8),dimension(:,:,:,:,:,:),optional             :: Fimp
  integer                                       :: i,ilat,Nineq,L
  logical                                       :: check
  complex(8),dimension(:,:,:,:,:,:),allocatable :: gf,ff
  !
  Nineq=Nlat
  L=size(zeta)
  !
  allocate(Gf(Nineq,Nspin,Nspin,Norb,Norb,L))
  allocate(Ff(Nineq,Nspin,Nspin,Norb,Norb,L))
  !
  do ilat = 1, Nineq
     ed_file_suffix=reg(ineq_site_suffix)//str(ilat,site_indx_padding)
     if(present(fimp))then
        call rebuild_gimp_single_n5(zeta,&
             gf(ilat,:,:,:,:,:),&
             ff(ilat,:,:,:,:,:))
     else
        call rebuild_gimp_single_n5(zeta,&
             ff(ilat,:,:,:,:,:))
     endif
  enddo
  call ed_reset_suffix
  !
  call assert_shape(gimp,[Nineq,Nspin,Nspin,Norb,Norb,L],'rebuild_gimp','gimp')
  gimp = gf
  if(present(fimp))then
     call assert_shape(fimp,[Nineq,Nspin,Nspin,Norb,Norb,L],'rebuild_gimp','fimp')
     fimp = ff
  endif
  return
end subroutine rebuild_gimp_ineq_n6





subroutine rebuild_gimp_normal(zeta,gf)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: gf
  complex(8),dimension(size(zeta))                       :: green
  integer                                                :: ispin,jspin
  integer                                                :: iorb,jorb
  integer                                                :: i
  integer                                                :: Nstates,istate
  integer                                                :: Nchannels,ichan
  integer                                                :: Nexcs,iexc
  real(8)                                                :: peso,de
  !
  gf = zero
  !
  do ispin=1,Nspin     
     do iorb=1,Norb
        do jorb=1,Norb
           green   = zero
           !
           if(.not.allocated(impGmatrix(ispin,ispin,iorb,jorb)%state)) cycle
           Nstates = size(impGmatrix(ispin,ispin,iorb,jorb)%state)
           do istate=1,Nstates
              if(.not.allocated(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel))cycle
              Nchannels = size(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel)
              do ichan=1,Nchannels
                 Nexcs  = size(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%poles)
                 if(Nexcs==0)cycle
                 do iexc=1,Nexcs
                    peso  = impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%weight(iexc)
                    de    = impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%poles(iexc)
                    do i=1,size(zeta)
                       green(i) = green(i) + peso/(zeta(i)-de)
                    enddo
                 enddo
              enddo
           enddo
           !
           gf(ispin,ispin,iorb,jorb,:) = green
        enddo
     enddo
  enddo
  if(offdiag_gf_flag)then
     do ispin=1,Nspin
        do iorb=1,Norb
           do jorb=1,Norb
              if(iorb==jorb)cycle
              gf(ispin,ispin,iorb,jorb,:) = 0.5d0*(gf(ispin,ispin,iorb,jorb,:) &
                   -gf(ispin,ispin,iorb,iorb,:) -gf(ispin,ispin,jorb,jorb,:))
           enddo
        enddo
     enddo
  end if
  !
end subroutine rebuild_gimp_normal



subroutine rebuild_gimp_superc(zeta,gf,ff)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: gf,ff,bg
  complex(8),dimension(4,size(zeta))                     :: auxG
  integer                                                :: ispin,jspin
  integer                                                :: iorb,jorb
  integer                                                :: Nstates,istate
  integer                                                :: Nchannels,ichan,ic
  integer                                                :: Nexcs,iexc
  real(8)                                                :: peso,de
  !
  gf = zero
  !
  ispin = 1
  !
  do iorb=1,Norb
     auxG = zero
     !
     if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state)) cycle
     Nstates = size(impGmatrix(ispin,ispin,iorb,iorb)%state)
     do istate=1,Nstates
        if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel))cycle
        Nchannels = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel)
        do ic=1,Nchannels
           Nexcs  = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%poles)
           if(Nexcs==0)cycle
           select case(ic)
           case(1,2);ichan=1
           case(3,4);ichan=2
           case(5:8);ichan=3
           end select
           do iexc=1,Nexcs
              peso  = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%weight(iexc)
              de    = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%poles(iexc)
              auxG(ichan,:)=auxG(ichan,:) + peso/(zeta-de)
           enddo
        enddo
     enddo
     !
     gf(ispin,ispin,iorb,iorb,:) = auxG(1,:)
     bg(ispin,ispin,iorb,iorb,:) = auxG(2,:)
     ff(ispin,ispin,iorb,iorb,:) = 0.5d0*(auxG(3,:)-auxG(1,:)-auxG(2,:))
     ! >ANOMAL
     ! ff(ispin,ispin,iorb,iorb,:) = 0.5d0*(auxG(3,:)-(one-xi)*auxG(1,:)-(one-xi)*auxG(2,:))
     ! <ANOMAL
  enddo
  !
  if(bath_type=='hybrid')then
     do iorb=1,Norb
        do jorb=1,Norb
           if(iorb==jorb)cycle
           auxG = zero
           !
           if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state)) cycle
           Nstates = size(impGmatrix(ispin,ispin,iorb,iorb)%state)
           do istate=1,Nstates
              if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel))cycle
              Nchannels = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel)
              do ic=1,Nchannels
                 Nexcs  = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%poles)
                 if(Nexcs==0)cycle
                 ichan=4
                 do iexc=1,Nexcs
                    peso  = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%weight(iexc)
                    de    = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ic)%poles(iexc)
                    auxG(ichan,:)=auxG(ichan,:) + peso/(zeta-de)
                 enddo
              enddo
           enddo
           !
           gf(ispin,ispin,iorb,jorb,:) = auxG(4,:) !This is G_oo -xi*G_pp = A_ab -xi*B_ab
        enddo
     enddo
     do iorb=1,Norb
        do jorb=1,Norb
           if(iorb==jorb)cycle
           ff(ispin,ispin,iorb,jorb,:) = 0.5d0*( gf(ispin,ispin,iorb,jorb,:) &
                -(one-xi)*gf(ispin,ispin,iorb,iorb,:) - (one-xi)*bg(ispin,ispin,jorb,jorb,:) )
        enddo
     enddo
  endif
  return
  !
end subroutine rebuild_gimp_superc



subroutine rebuild_gimp_nonsu2(zeta,gf)
  complex(8),dimension(:)                                :: zeta
  complex(8),dimension(Nspin,Nspin,Norb,Norb,size(zeta)) :: gf
  complex(8),dimension(size(zeta))                       :: green
  integer                                                :: ispin,jspin
  integer                                                :: iorb,jorb
  integer                                                :: Nstates,istate
  integer                                                :: Nchannels,ichan
  integer                                                :: Nexcs,iexc
  real(8)                                                :: peso,de
  !
  gf = zero
  !
  !same orbital, same spin GF: G_{aa}^{ss}(z)
  do ispin=1,Nspin
     do iorb=1,Norb
        !
        green   = zero
        if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state)) cycle
        Nstates = size(impGmatrix(ispin,ispin,iorb,iorb)%state)
        do istate=1,Nstates
           if(.not.allocated(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel))cycle
           Nchannels = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel)
           do ichan=1,Nchannels
              Nexcs  = size(impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ichan)%poles)
              if(Nexcs==0)cycle
              do iexc=1,Nexcs
                 peso  = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ichan)%weight(iexc)
                 de    = impGmatrix(ispin,ispin,iorb,iorb)%state(istate)%channel(ichan)%poles(iexc)
                 green = green + peso/(zeta-de)
              enddo
           enddo
        enddo
        gf(ispin,ispin,iorb,iorb,:) = green
        !
     enddo
  enddo
  !
  !
  do ispin=1,Nspin
     do jspin=1,Nspin
        if(ispin==jspin)cycle
        do iorb=1,Norb
           !
           green   = zero
           if(.not.allocated(impGmatrix(ispin,jspin,iorb,iorb)%state)) cycle
           Nstates = size(impGmatrix(ispin,jspin,iorb,iorb)%state)
           do istate=1,Nstates
              if(.not.allocated(impGmatrix(ispin,jspin,iorb,iorb)%state(istate)%channel))cycle
              Nchannels = size(impGmatrix(ispin,jspin,iorb,iorb)%state(istate)%channel)
              do ichan=1,Nchannels
                 Nexcs  = size(impGmatrix(ispin,jspin,iorb,iorb)%state(istate)%channel(ichan)%poles)
                 if(Nexcs==0)cycle
                 do iexc=1,Nexcs
                    peso  = impGmatrix(ispin,jspin,iorb,iorb)%state(istate)%channel(ichan)%weight(iexc)
                    de    = impGmatrix(ispin,jspin,iorb,iorb)%state(istate)%channel(ichan)%poles(iexc)
                    green = green + peso/(zeta-de)
                 enddo
              enddo
           enddo
           gf(ispin,jspin,iorb,iorb,:) = green
           !
        enddo
     enddo
  enddo
  do ispin=1,Nspin
     do jspin=1,Nspin
        if(ispin==jspin)cycle
        do iorb=1,Norb
           gf(ispin,jspin,iorb,iorb,:) = 0.5d0*(gf(ispin,jspin,iorb,iorb,:) &
                -(one+xi)*gf(ispin,ispin,iorb,iorb,:) -(one+xi)*gf(jspin,jspin,iorb,iorb,:))
        enddo
     enddo
  enddo
  !
  !
  !
  !
  select case(bath_type)
  case default;
  case("hybrid","replica","general")
     do ispin=1,Nspin
        do iorb=1,Norb
           do jorb=1,Norb
              if(iorb==jorb)cycle
              !
              green   = zero
              if(.not.allocated(impGmatrix(ispin,ispin,iorb,jorb)%state)) cycle
              Nstates = size(impGmatrix(ispin,ispin,iorb,jorb)%state)
              do istate=1,Nstates
                 if(.not.allocated(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel))cycle
                 Nchannels = size(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel)
                 do ichan=1,Nchannels
                    Nexcs  = size(impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%poles)
                    if(Nexcs==0)cycle
                    do iexc=1,Nexcs
                       peso  = impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%weight(iexc)
                       de    = impGmatrix(ispin,ispin,iorb,jorb)%state(istate)%channel(ichan)%poles(iexc)
                       green = green + peso/(zeta-de)
                    enddo
                 enddo
              enddo
              gf(ispin,ispin,iorb,jorb,:) = green
              !
           enddo
        enddo
     enddo
     do ispin=1,Nspin
        do iorb=1,Norb
           do jorb=1,Norb
              if(iorb==jorb)cycle
              gf(ispin,ispin,iorb,jorb,:) = 0.5d0*(gf(ispin,ispin,iorb,jorb,:) &
                   -(one+xi)*gf(ispin,ispin,iorb,iorb,:) -(one+xi)*gf(ispin,ispin,jorb,jorb,:))
           enddo
        enddo
     enddo
     !
     !
     do ispin=1,Nspin
        do jspin=1,Nspin
           if(ispin==jspin)cycle
           do iorb=1,Norb
              do jorb=1,Norb
                 if(iorb==jorb)cycle
                 !
                 green   = zero
                 if(.not.allocated(impGmatrix(ispin,jspin,iorb,jorb)%state)) cycle
                 Nstates = size(impGmatrix(ispin,jspin,iorb,jorb)%state)
                 do istate=1,Nstates
                    if(.not.allocated(impGmatrix(ispin,jspin,iorb,jorb)%state(istate)%channel))cycle
                    Nchannels = size(impGmatrix(ispin,jspin,iorb,jorb)%state(istate)%channel)
                    do ichan=1,Nchannels
                       Nexcs  = size(impGmatrix(ispin,jspin,iorb,jorb)%state(istate)%channel(ichan)%poles)
                       if(Nexcs==0)cycle
                       do iexc=1,Nexcs
                          peso  = impGmatrix(ispin,jspin,iorb,jorb)%state(istate)%channel(ichan)%weight(iexc)
                          de    = impGmatrix(ispin,jspin,iorb,jorb)%state(istate)%channel(ichan)%poles(iexc)
                          green = green + peso/(zeta-de)
                       enddo
                    enddo
                 enddo
                 gf(ispin,jspin,iorb,jorb,:) = green
              enddo
           enddo
        enddo
     enddo
     do ispin=1,Nspin
        do jspin=1,Nspin
           if(ispin==jspin)cycle
           do iorb=1,Norb
              do jorb=1,Norb
                 if(iorb==jorb)cycle
                 gf(ispin,jspin,iorb,jorb,:) = 0.5d0*(gf(ispin,jspin,iorb,jorb,:) &
                      -(one+xi)*gf(ispin,ispin,iorb,iorb,:) -(one+xi)*gf(jspin,jspin,jorb,jorb,:))
              enddo
           enddo
        enddo
     enddo
  end select
  !
  return
  !
end subroutine rebuild_gimp_nonsu2
