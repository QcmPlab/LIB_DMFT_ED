subroutine ed_get_neigen_total(nlii,Nlat) 
  integer                      :: Nlat
  integer,dimension(Nlat) :: nlii
  nlii=0d0
  if(allocated(neigen_total_ineq))then
     if(Nlat>size(neigen_total_ineq)) stop "ed_get_docc error: required N_sites > evaluated N_sites"
     nlii=neigen_total_ineq
  endif
end subroutine ed_get_neigen_total


