! ============================================================================
! Name        : easter.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Chritian Calendar in Fortran
! ============================================================================

!###########################################################
!# calculate the date of Easter given the current year
!#
!#
!###########################################################
module christiancal
implicit none

   type cDate
     real :: day
     integer :: month
     integer :: year
   end type cDate


contains
   !====================================================
   != dis_date
   !====================================================
   function dis_date(day, mon, yr)

     integer :: mon,yr
     real :: day
     character(len=10) :: dis_date
     character(len=20) :: smon
     character(len=20) :: sday
     character(len=20) :: syr

     write(smon, *) mon
     write(sday, *) int(day)
     write(syr, *) yr

     dis_date =adjustl(trim(smon)// "/" // adjustl(trim(sday)// "/" // adjustl(trim(syr))))


   end function dis_date

   !====================================================
   != dis_cDate
   !====================================================
   function dis_cDate(dateIn)

     type(cDate) :: dateIn
     character(len=10) :: dis_cDate
     character(len=20) :: smon
     character(len=20) :: sday
     character(len=20) :: syr

     write(smon, *) dateIn%month
     write(sday, *) int(dateIn%day)
     write(syr, *) dateIn%year

     dis_cDate =adjustl(trim(smon)// "/" // adjustl(trim(sday)// "/" // adjustl(trim(syr))))


   end function dis_cDate

   !====================================================
   != easter
   !====================================================
   subroutine easterSunday(day, mon, yr)
   implicit none

     integer :: mon,yr
     real :: day, eday

     integer :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,p
     integer :: yearin



     yearin = yr


     !Step 1
     a = mod(yearin,19)

     !Step 2
     b = yearin / 100
     c = mod(yearin, 100)

     !Step 3
     d = b / 4
     e = mod(b,4)

     !Step 4
     f = (b+8)/25

     !Step 5
     g = (b-f+1)/3

     !Step 6
     h = mod((19*a+b-d-g+15),30)

     !Step 7
     i = c / 4
     k = mod(c,4)

     !Step 8
     l = mod((32+2*e+2*i-h-k),7)

     !Step 9
     m = (a+11*h+22*l)/451

     !Step 10
     n = (h+l-7*m+114)/31
     p = mod((h+l-7*m+114),31)

     !Step 11
     eday = p + 1

     !Return values
     day = eday
     mon = n


   end subroutine easterSunday


   !====================================================
   != julian_date
   !====================================================
   function julian_date(day, mon, yr)

     integer ::  mon, yr
     real :: julian_date,day
     real :: m_prime, y_prime
     logical :: flg_gregorian_date
     integer :: A, B, C, D


     !Step 1
     if(mon == 1 .OR. mon == 2) then
       y_prime = yr - 1
       m_prime = mon + 12
     else
       y_prime = yr
       m_prime = mon
     end if


     !Step 2
     flg_gregorian_date = .TRUE.

     if (yr > 1582) then
       ! No Action
     else if (yr < 1582) then
       flg_gregorian_date = .FALSE.
     else if (yr == 1582) then
       if (mon > 10) then
          !no Action
       else if (mon < 10) then
          flg_gregorian_date = .FALSE.
       else if (mon == 10) then
          if (day <= 15) then
             flg_gregorian_date = .FALSE.
          end if
       end if
     end if

     if(flg_gregorian_date .eqv. .TRUE.) then
       A = int(y_prime / 100)
       B = 2 - A + int(A/4)
     else
       B = 0
     end if

     !Step 3
       if(y_prime < 0) then
         C = int(365.25 * y_prime) - .75
       else
         C = int(365.25 * y_prime)
       end if


     !Step 4
       D = int(30.6001 * (m_prime + 1))

     !Step 5
       julian_date = B + C + D + day + 1720994.5


   end function julian_date

   !====================================================
   != cal_date
   !====================================================
   function cal_date(juliandate)

     real :: juliandate
     type(cDate) :: cal_date

     integer :: I, A, B, C, D, E, G
     real ::  F, m,jd

     !Step 1
     jd = juliandate + .5
     I = int(jd)
     F = jd - I

     !Step 2
     if (I > 2299160) then
       A = int((I - 1867216.25)/36524.25)
       B = I  + 1 + A - int(A/4)
     else
       B = 1
     end if

     !step 3
     C = B + 1524

     !Step 4
     D = int((C - 122.1)/365.25)

     !Step 5
     E = int(365.25 * D)

     !Step 6
     G = ((C - E)/30.6001)

     !Step 7
     cal_date%day = C - E + F - int(30.6001 * G)

     !Step 8
     if (G > 13.5) then
       cal_date%month = G - 13
       m = G - 13
     else
       cal_date%month = G - 1
       m = G - 1
     end if

     !Step 9
     if (m > 2.5) then
       cal_date%year = D - 4716
     else
       cal_date%year = D - 4715
     end if

   end function cal_date

   !====================================================
   != cal_date
   !====================================================
   function day_of_week(juliandate)

     real :: juliandate, A, A_prime
     integer :: day_of_week

     A = (juliandate + 1.5) / 7

     A_prime = (A - int(A)) * 7

     day_of_week = nint(A_prime)

   end function day_of_week

end module christiancal


!================================================
! Compile as : gfortran -o easter easter1.f03 christianCal.f03
!================================================
program easter
use christiancal
  implicit none

   character(20) :: a_string
   character(20) :: a_display
   integer :: a_number, fourth_offset, king_offset, halloween_offset
   type(cDate) :: easter_date, ash, good, maundy, ascension, palm, christmas,fourth,king

   integer :: mon,yr
   real :: day, easter_julian,christmas_julian , fourth_julian,king_julian,king_julian_prime

   real :: halloween_julian, reform_julian, pentecost_julian, trinity_julian, all_saints_julian


   !Clear screen
   print *, achar(27)//"[2J"

   !Enter Year
   print *,"Enter year :"
   read *,yr


   !Calculate Offsets
   halloween_julian = julian_date(31.0,10,yr+1)
   halloween_offset = day_of_week(halloween_julian)
   reform_julian = halloween_julian - halloween_offset
   all_saints_julian = reform_julian + 7





   christmas%day = 25
   christmas%month = 12
   christmas%year = yr
   christmas_julian = julian_date(christmas%day,christmas%month,christmas%year)

   !Calculate Easter Date
   call easterSunday(day, mon, yr+1)


   easter_date%day = day
   easter_date%month = mon
   easter_date%year = yr+1

   easter_julian = julian_date(easter_date%day,easter_date%month,easter_date%year)



   !Ash Wednesday
   ash = cal_date(easter_julian - 46)

   !Palm Sunday
   palm = cal_date(easter_julian - 7)

   !Maundy Thursday
   maundy = cal_date(easter_julian - 3)


   !Good Friday
   good = cal_date(easter_julian - 2)

   !Ascension
   ascension = cal_date(easter_julian + 39)

   !Pentecost
   pentecost_julian = easter_julian + 49


   !Trinity
   trinity_julian = easter_julian + 56


   !Fourth Advent of Christmas
   fourth_offset =  day_of_week(christmas_julian)
   if (fourth_offset == 0) then
     fourth_offset = 7
   end if
   fourth_julian = christmas_julian - fourth_offset

   !Christ the King
   king_julian_prime = julian_date(christmas%day,christmas%month,christmas%year + 1)
   king_offset = day_of_week(king_julian_prime)
   if (king_offset == 0) then
     king_offset = 7
   end if
   king_julian = king_julian_prime - king_offset  -28


   !Clear screen
   print *, achar(27)//"[2J"


   !print *, day_of_week(2446113.5)

   print *, "================================================================"
   print *, "Christain Calendar for the Year starting in : ", yr
   print *, "================================================================"
   print*,"=="


   print*,"== First Advent    : ",dis_cDate(cal_date(fourth_julian - 21))
   print*,"== Second Advent   : ",dis_cDate(cal_date(fourth_julian - 14))
   print*,"== Third Advent    : ",dis_cDate(cal_date(fourth_julian - 7))
   print*,"== Fourth Advent   : ",dis_cDate(cal_date(fourth_julian))


   print*,"== Christmas Eve   : ",dis_cDate(cal_date(christmas_julian - 1))
   print*,"== Christmas Day   : ",dis_cDate(christmas)


   print*,"== Ash Wednesday   : ",dis_cDate(ash)
   print*,"== Palm Sunday     : ",dis_cDate(palm)
   print*,"== Maundy Thursday : ",dis_cDate(maundy)
   print*,"== Good Friday     : ",dis_cDate(good)
   print*,"== Easter          : ",dis_cDate(easter_date)
   print*,"== Ascension       : ",dis_cDate(ascension)

   print*,"== Pentecost       : ",dis_cdate(cal_date(pentecost_julian))
   print*,"== Trinity Sunday  : ",dis_cdate(cal_date(trinity_julian))

   print*,"== Reformation     : ",dis_cDate(cal_date(reform_julian))

   print*,"== All Saints      : ",dis_cDate(cal_date(all_saints_julian))

   print*,"== Christ the King : ",dis_cDate(cal_date(king_julian))


   print*,"=="
   print *, "================================================================"
   print*,""
   print*,""


!   print *,day_of_week(julian_date(17.25, 2, 1985))

end program easter





