! ============================================================================
! Name        : fe.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

program fe
    implicit none
    integer :: ierror
    character(len=80):: label

    !open(unit=13,file="test.dat",status="replace",action="write",iostat=ierror)
    open(unit=13,file="test.dat",status="old",action="read",iostat=ierror)

    if(ierror /= 0) then
      print*,"Failed to open test.date"
      stop
    end if

    !write(unit=13,fmt=*) "hello there world"
    read(unit=13,fmt="(a80)") label
    print*,label

    read(unit=13,fmt="(a80)") label

    print*,label



    close (unit=13)

end program
