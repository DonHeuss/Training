! ============================================================================
! Name        : TestInput.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

program TestInput
    implicit none

    character (len=15) :: first_name
    real :: x, y
    x = 1.2225

    print *, "Enter"
    read *,first_name
    print *,"name: ",first_name

    print *, "enter:"
    read "(f8.2,f8.2)", x,y
    print "(2(f8.3))",x,y


    !Write to file





end program
