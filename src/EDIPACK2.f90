MODULE EDIPACK2
  USE ED_INPUT_VARS  , only: &
       ed_read_input , &
       ed_update_input,&
       Norb          , &
       Nspin         , &
       Nbath         , &
       Nloop         , &
       Nph           , &
       Uloc          , &
       Ust           , &
       Jh            , &
       Jx            , &
       Jp            , &
       xmu           , &
       beta          , &
       g_ph          , &
       w0_ph         , &
       eps           , &
       wini          , &
       wfin          , &
       xmin          , &
       xmax          , &
       Nsuccess      , &
       dmft_error    , &
       sb_field      , &
       cg_Scheme     , &
       nread         , &
       Lmats         , &
       Lreal         , &
       Lpos          , &
       Hfile         , &
       HLOCfile      , &
       LOGfile       , &
       ed_mode       , &
       ed_verbose    , &
       ed_hw_bath    , &
       bath_type 

  USE ED_BATH, only:                                                    &
       ed_set_Hreplica                 => set_Hreplica                 , &
       ed_Hreplica_mask                => Hreplica_mask                , &
       ed_set_Hgeneral                 => set_Hgeneral                 , &
       ed_Hgeneral_mask                => Hgeneral_mask                , &
       ed_get_bath_dimension           => get_bath_dimension           , &
       ed_spin_symmetrize_bath         => spin_symmetrize_bath         , &
       ed_orb_symmetrize_bath          => orb_symmetrize_bath          , &
       ed_orb_equality_bath            => orb_equality_bath            , &
       ed_ph_symmetrize_bath           => ph_symmetrize_bath           , &
       ed_ph_trans_bath                => ph_trans_bath                , &
       ed_break_symmetry_bath          => break_symmetry_bath          , &
       ed_enforce_normal_bath          => enforce_normal_bath


  USE ED_AUX_FUNX, only:                        &
       ed_set_Hloc                                                       , &
       ed_set_suffix                                                     , &
       ed_reset_suffix                                                   , &
       ed_search_variable                                                , &
       ed_search_chemical_potential     => search_chemical_potential


  USE ED_IO, only: &
       ed_get_gimp            , &
       ed_get_sigma           , &
       ed_get_g0imp           , &
       ed_get_g0and           , &
       ed_get_delta           , &
       ed_get_dens            , &
       ed_get_mag             , &
       ed_get_docc            , &
       ed_get_eimp            , &
       ed_get_epot            , &
       ed_get_eint            , &
       ed_get_ehartree        , &
       ed_get_eknot           , &
       ed_get_doubles         , &
       ed_get_density_matrix


  USE ED_MAIN, only:    &
       ed_init_solver , &
       ed_rebuild_gf  , &
       ed_solve

  USE ED_BATH_FIT,  only: ed_chi2_fitgf


END MODULE EDIPACK2

