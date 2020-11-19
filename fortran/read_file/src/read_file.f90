! ============================================================================
! Name        : write_file.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

program write_file
    implicit none

    integer, parameter:: linelength = 120
    character(len=linelength)::line
    integer :: ierror

    open(unit=13,file="D:\fortran\test1.dat",status="old",action="read",iostat=ierror)

    if(ierror /= 0) then
      print*,"Failed to open test.dat"
      stop
    end if

    readfile : do
       read(unit=13,fmt="(a)", iostat=ierror) line

       if(ierror<0) then
         print*, "end of file reached"
         exit readfile

       else if (ierror > 0) then
         print*, "Error during read"
         exit readfile

       else
         !if(index(line,"energy")>0) then
         !  print*, "energy found"
         !  exit readfile
         !end if

         print *, line


       end if


    end do readfile

    close(unit=13)

end program
