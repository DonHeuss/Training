! ============================================================================
! Name        : vertical.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

program vertical
    implicit none
    real::g
    real::s
    real::t
    real::u

    ! set value of varialbles
    g = 9.8
    t = 6.0
    u = 60

    s = u * t - g * (t**2) / 2

    write(*,*) 'Time = ', t, '    Displacement = ', s
end program
