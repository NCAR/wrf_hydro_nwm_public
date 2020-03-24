SUBROUTINE ESMF_VMDefault(vm)

  use ESMF
  type(ESMF_VM):: vm

  
  ! local variables
  integer:: rc
  integer:: localPet, petCount, peCount, ssiId, vas


  call ESMF_VMPrint(vm, rc=rc)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, &
                  peCount=peCount, rc=rc)
  print *, "This PET is localPet: ", localPet
  print *, "of a total of ",petCount," PETs in this VM."
  print *, "There are ", peCount," PEs referenced by this VM"

  ! call ESMF_VMGetPETLocalInfo(vm, localPet, peCount=peCount, &
  !                             ssiId=ssiId, vas=vas, rc=rc)
  ! print *, "This PET is executing in virtual address space (VAS) ", vas
  ! print *, "located on single system image (SSI) ", ssiId
  ! print *, "and is associated with ", peCount, " PEs."

END
