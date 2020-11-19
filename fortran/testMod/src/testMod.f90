! ============================================================================
! Name        : testMod.f90
! Author      : Don
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================


module modAstroCalc
implicit none

  ! Declare local constant Pi
  REAL, PARAMETER :: Pi = 3.1415927

  ! Obliquity of ecliptic
  REAL, PARAMETER :: e_ec = 23.446236  !degrees
  REAL, PARAMETER :: e_ec_rad = 0.409214015  !radians

  !The Sun's mean ecliptic longitude at the epoch 279.403303
  REAL, PARAMETER :: s_ec_lon_epoch = 279.403303 !Degrees

  !The Sun's ecliptic longitude of perigee 282.768422
  REAL, PARAMETER :: s_ec_lon_perigee = 282.768422 !Degrees

  !Eccentricity of orbit
  REAL, PARAMETER :: e_orb = 0.016713

  !Time with decimal seconds
  type astrotime
    integer :: hr
    integer :: min
    real    :: sec
  end type astrotime

  !Calendar Date
  type cDate
     real    :: day
     integer :: month
     integer :: year
  end type cDate

  !Formal Measurement in degrees,minutes,seconds
  type formal_deg
    integer :: degree
    integer :: minute
    real    :: second
  end type

  !Local timezone in degrees
  real :: local_time_zone

  !============
  !Coordinate system - all values are radians
  !============
  type horizon
    real :: az       !azimuth
    real :: al       !altitude
  end type horizon

  type equatorial
    real :: ra       !right_accention
    real :: dec      !declination
  end type equatorial

  type ecliptic
    real :: la       !latitude
    real :: lon      !longitude
  end type ecliptic

  type galactic
    real :: la       !latitude
    real :: lon      !longitude
  end type galactic

  type coordinates
    type(horizon)    :: hz
    type(equatorial) :: eq
    type(ecliptic)   :: ec
    type(galactic)   :: ga

  end type coordinates

  !============
  !Rise and set
  !============
  type equatorial_rs
    real :: A        !Azimuth in radians
    real :: LST      !Local Siderial time in decimal hours
  end type equatorial_rs

  type body_rise_set
    type(equatorial_rs) :: rise
    type(equatorial_rs) :: set
  end type body_rise_set

contains
   !====================================================
   != Sec 4
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
   != Sec 5
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
   != Sec 6
   != day_of_week
   !====================================================
   function day_of_week(juliandate)

     real :: juliandate, A, A_prime
     integer :: day_of_week

     A = (juliandate + 1.5) / 7

     A_prime = (A - int(A)) * 7

     day_of_week = nint(A_prime)

   end function day_of_week




   !====================================================
   != Sec 7
   != stod
   != Description : Standard Time to Decimal Time
   !====================================================
   function stod(hr, min, sec)
   implicit none
     real :: sec, rreturn, dsec, dhr, dmin, stod
     integer :: hr, min

     rreturn = 0

     !Step 1
     dsec = sec/60

     !Step 2
     dmin = (min + dsec)/60

     !Step 3
     dhr = hr + dmin
     rreturn = dhr

     stod = rreturn

   end function stod


   !====================================================
   != Sec 8
   != dtos
   != Description : Decimal to standard time
   !====================================================
   function dtos(timein)
   implicit none
    type(astrotime) ::dtos
    integer :: hr, min
    real :: timein, sec, min_prime, sec_prime

    hr = INT(timein)
    min_prime = timein -  hr
    min = INT(min_prime * 60)
    sec_prime = (min_prime * 60) - min

    sec = sec_prime * 60


    dtos%hr = hr
    dtos%min = min
    dtos%sec = sec


   end function dtos

   !====================================================
   != Sec 12
   != UT_to_GST
   !====================================================
   function UT_to_GST(jd_in,ut_in)

     real :: UT_to_GST, jd_in, ut_in
     real(8) :: S, T, T0, UT, GST, T1

     !Step 1
     S = jd_in - 2451545.0
!print *,S

     !Step 2
     T = NINT((S / 36525.0) * 10000000.0) / 10000000.0


     !T = 0.196947

     !Step 3
     T0 = ((6.697374558 * 100000000.0) + ((2400.051336 * 100000000.0) * T)  + (0.000025862 * 100000000.0  * (T*T))) /  100000000.0

!print *,"T0 : ", T0
     if(T0 > 24) then
       do while (T0 > 24)
         T0 = T0 - 24.0
       end do
     end if

     if(T0 < 0) then
       do while (T0 < 0)
         T0 = T0 + 24.0
       end do
     end if


     !Step 4
     UT = ((ut_in * 100000000.0) * 1.002737909) / 100000000.0

     !Step 5
     T1 = ((T0 * 100000000.0)+ (UT * 100000000.0)) / 100000000.0

     !Step 6
     if (T1 > 24) then
       T1 = T1 - 24
     end if

     if (T1 <= 0) then
       T1 = T1 + 24
     end if

     !Step 7
     UT_to_GST = T1


   end function UT_to_GST

   !====================================================
   != Sec 13
   != GST_to_UT
   !====================================================
   function GST_to_UT(jd_in,gst_in)

     real :: GST_to_UT, jd_in, gst_in
     real(8) :: S, T, T0, T1

     !Step 1
     S = jd_in - 2451545.0

     !Step 2
     T = S/36525.0
!print *,"S : ", S

     !Step 3
     T0 = ((6.697374558 * 100000000.0)  + ((2400.051336 * 100000000.0) * T ) + ((0.000025862 * 100000000.0) * T**2))  / 100000000.0
!print *,"T0 : ", T0

     if(T0 > 24) then
       do while (T0 > 24)
         T0 = T0 - 24.0
       end do
     end if

     if(T0 < 0) then
       do while (T0 < 0)
         T0 = T0 + 24.0
       end do
     end if
!print *,"T0 : ", T0


     !Step 4
     T1 = gst_in - T0

     !Step 5
     if (T1 > 24) then
       T1 = T1 - 24
     end if

     if (T1 <= 0) then
       T1 = T1 + 24
     end if
!print *,"T1 : ", T1

     !Step 6
     T1 = (T1 * (0.9972695663 * 100000000.0)) / 100000000.0

     !Step 7
     GST_to_UT = T1
   end function GST_to_UT


   !====================================================
   != Sec 14
   != LST
   !====================================================
   function LST(jd_in,longitude)
     real :: LST, jd_in, longitude, dtime

     dtime = longitude / 15

     LST = jd_in + dtime
     if (LST > 24.0) then
       LST = LST - 24.0
     end if

     if(LST < 0) then
       LST = LST + 24.0
     end if

   end function LST

   !====================================================
   != Sec 15
   != LST_to_GST
   !====================================================
   function LST_to_GST(jd_in,longitude)
     real :: LST_to_GST, jd_in, longitude, dtime

     dtime = longitude / 15

     LST_to_GST = jd_in - dtime

     if (LST_to_GST > 24.0) then
       LST_to_GST = LST_to_GST - 24.0
     end if

     if(LST_to_GST < 0) then
       LST_to_GST = LST_to_GST + 24.0
     end if

   end function LST_to_GST


   !====================================================
   != Sec 24a  - Hour angle to Right Accension
   != RA_TO_HA
   !====================================================
   function RA_TO_HA(LST, RA)
      real :: RA_TO_HA, LST, RA, H
      H = ((LST * 1000000) - (RA * 1000000)) / 1000000
      if(H <= 0) then
        H = H + 24
      end if
      RA_TO_HA = H
   end function RA_TO_HA

   !====================================================
   != Sec 24b  - Right Accension to Hour angle
   != HA_TO_RA
   !====================================================
   function HA_TO_RA(LST, H)
      real :: HA_TO_RA, LST, H, RA
      RA = ((LST * 1000000) - (H * 1000000)) / 1000000
      if(RA <= 0) then
        RA = RA + 24
      end if
      HA_TO_RA = RA
   end function HA_TO_RA

   !====================================================
   != Sec 31
   != coor_conv
   !====================================================
   function coor_conv(coor_x, coor_y, lst, gl_rad, coor_type)

     real :: coor_x, coor_y
     real :: lst, lst_rad                       !local siderial time
     real :: gl_rad                             !Geographical latitude
     character(2) :: coor_type
     type(coordinates) :: coor_conv
     real, dimension(3,1) :: v, s, r, w
     real, dimension(3,3) :: A, B, C, C_prime, D, D_prime
     real :: x, y, z
     real :: m, n, p
     real :: new_coor_a, new_coor_b

     integer i

     !=========================
     !Matrix Definition
     !=========================
     !####
     !A
     !####
     !Geographical Latitude
     A(1,1) = -sin(gl_rad)
     A(1,2) = 0
     A(1,3) = cos(gl_rad)

     A(2,1) = 0
     A(2,2) = -1
     A(2,3) = 0

     A(3,1) = cos(gl_rad)
     A(3,2) = 0
     A(3,3) = sin(gl_rad)

     !####
     !B
     !####
     !Local Siderial Time
     lst_rad = deg_to_rad(lst * 15)

     B(1,1) = cos(lst_rad)
     B(1,2) = sin(lst_rad)
     B(1,3) = 0

     B(2,1) = sin(lst_rad)
     B(2,2) = -cos(lst_rad)
     B(2,3) = 0

     B(3,1) = 0
     B(3,2) = 0
     B(3,3) = 1


     !####
     !C
     !####
     C(1,1) = 1
     C(1,2) = 0
     C(1,3) = 0

     C(2,1) = 0
     C(2,2) = cos(e_ec_rad)
     C(2,3) = sin(e_ec_rad)

     C(3,1) = 0
     C(3,2) = -sin(e_ec_rad)
     C(3,3) = cos(e_ec_rad)


     !####
     !C'
     !####
     C_prime(1,1) = 1
     C_prime(1,2) = 0
     C_prime(1,3) = 0

     C_prime(2,1) = 0
     C_prime(2,2) = cos(e_ec_rad)
     C_prime(2,3) = -sin(e_ec_rad)

     C_prime(3,1) = 0
     C_prime(3,2) = sin(e_ec_rad)
     C_prime(3,3) = cos(e_ec_rad)



     !####
     !D
     !####
     D(1,1) = -0.0669887
     D(1,2) = -0.8727558
     D(1,3) = -0.4835389

     D(2,1) = 0.4927285
     D(2,2) = -0.4503470
     D(2,3) = 0.7445846

     D(3,1) = -0.8676008
     D(3,2) = -0.1883746
     D(3,3) = 0.4601998

     !####
     !D'
     !####
     D_prime(1,1) = -0.0669887
     D_prime(1,2) = 0.4927285
     D_prime(1,3) = -0.8676008

     D_prime(2,1) = -0.8727558
     D_prime(2,2) = -0.4503470
     D_prime(2,3) = -0.1883746

     D_prime(3,1) = -0.4835389
     D_prime(3,2) = 0.7445846
     D_prime(3,3) = 0.4601998



     !=========================
     !Calculate v
     !=========================
     !input Coordinates rendering
     x = cos(coor_x) * cos(coor_y)
     y = sin(coor_x) * cos(coor_y)
     z = sin(coor_y)

     v(1,1) = x
     v(2,1) = y
     v(3,1) = z



     !=========================
     !Calculate Coordinate systems
     !=========================

     !========
     !horizon
     !========
     if(coor_type == "hz") then
       coor_conv%hz%az  = coor_x
       coor_conv%hz%al = coor_y

       !===
       !equatorial
       !===
       !Matrix Calculations
       w = matmul(A,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%eq%ra = atan_cor_quad(m,n)
       coor_conv%eq%dec = asin(p)

       !===
       !ecliptic
       !===
       !Matrix Calculations
       s = matmul(A,v)
       r = matmul(B,s)
       w = matmul(C,r)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ec%la  = atan_cor_quad(m,n)
       coor_conv%ec%lon = asin(p)

       !===
       !galactic
       !===
       !Matrix Calculations
       s = matmul(A,v)
       r = matmul(B,s)
       w = matmul(D,r)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ga%la = atan_cor_quad(m,n)
       coor_conv%ga%lon = asin(p)

     !========
     !equatorial
     !========
     else if(coor_type == "eq") then   !equatorial
       coor_conv%eq%ra = coor_x
       coor_conv%eq%dec = coor_y

       !===
       !horizon
       !===
       !Matrix Calculations
       !s = matmul(B,v)
       w = matmul(A,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%hz%az  = atan_cor_quad(m,n)
       coor_conv%hz%al = asin(p)

       !===
       !ecliptic
       !===
       !Matrix Calculations
       w = matmul(C,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ec%la  = atan_cor_quad(m,n)
       coor_conv%ec%lon = asin(p)

       !===
       !galactic
       !===
       !Matrix Calculations
       w = matmul(D,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ga%la = atan_cor_quad(m,n)
       coor_conv%ga%lon = asin(p)

     !========
     !ecliptic
     !========
     else if(coor_type == "ec") then
       coor_conv%ec%la  = coor_x
       coor_conv%ec%lon = coor_y

       !===
       !horizon
       !===
       !Matrix Calculations
       s = matmul(C_prime,v)
       r = matmul(B,s)
       w = matmul(A, r)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%hz%az = atan_cor_quad(m,n)
       coor_conv%hz%al = asin(p)


       !===
       !equatorial
       !===
       !Matrix Calculations
       w = matmul(C_prime,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%eq%ra = atan_cor_quad(m,n)
       coor_conv%eq%dec = asin(p)

       !===
       !galactic
       !===
       !Matrix Calculations
       s = matmul(C_prime,v)
       w = matmul(D,s)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ga%la = atan_cor_quad(m,n)
       coor_conv%ga%lon = asin(p)


!DFH
      m = s(1,1)
       n = s(2,1)
       p = s(3,1)

print *, "right accention =", rad_to_deg(atan_cor_quad(m,n))
print *, "declination = ", rad_to_deg(asin(p))

     !========
     !galactic
     !========
     else if(coor_type == "ga") then   !galactic
       coor_conv%ga%la = coor_x
       coor_conv%ga%lon = coor_y

       !===
       !horizon
       !===
       !Matrix Calculations
       s = matmul(D_prime,v)
       r = matmul(B,s)
       w = matmul(A,r)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%hz%az = atan_cor_quad(m,n)
       coor_conv%hz%al = asin(p)

       !===
       !equatorial
       !===
       !Matrix Calculations
       w = matmul(D_prime,v)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%eq%ra = atan_cor_quad(m,n)
       coor_conv%eq%dec = asin(p)

       !===
       !ecliptic
       !===
       !Matrix Calculations
       s = matmul(D_prime,v)
       w = matmul(C,s)

       !coordinates rendering
       m = w(1,1)
       n = w(2,1)
       p = w(3,1)

       !Output coordinate in radians
       coor_conv%ec%la  = atan_cor_quad(m,n)
       coor_conv%ec%lon = asin(p)


     end if


!--------------------------------------------



!print *, rad_to_deg(new_coor_a)
!print *, rad_to_deg(new_coor_b)
!---------------------------------------------------


   end function coor_conv

   !====================================================
   != Sec 32 - The angle between two celestial objects
   != ang_btn_obj
   != Input : coor_1, coor_2 as equatorial coordinates
   != Output : radian
   !====================================================
   function ang_btn_obj(coor_1, coor_2)
      real :: ang_btn_obj, delta_ra, d
      type(equatorial) :: coor_1, coor_2

      d = sin(coor_1%dec) * sin(coor_2%dec) + cos(coor_1%dec) * cos(coor_2%dec) * cos(coor_1%ra - coor_2%ra)

      ang_btn_obj = acos(d)
   end function ang_btn_obj

   !====================================================
   != Sec 33 - Rising and Setting
   != rise_set
   != Input :
   !====================================================
   function rise_set(eq_in,geo_lat)

     type(body_rise_set)  :: rise_set
     type(equatorial) :: eq_in
     real :: geo_lat,LST_rise, LST_set
     real :: A_rise, A_set, H

     A_rise = acos(sin(eq_in%dec)/cos(geo_lat))
     A_set = (2 * Pi) -  A_rise

     H = acos(-tan(geo_lat) * tan(eq_in%dec))

     LST_rise = 24 + degree_to_hours(rad_to_deg(eq_in%ra - H))
     if(LST_rise > 24.0) then
       LST_rise = LST_rise - 24.0
     end if

     rise_set%rise%A =  A_rise
     rise_set%rise%LST = LST_rise

     LST_set = degree_to_hours(rad_to_deg(eq_in%ra + H))
     if(LST_set > 24.0) then
       LST_set = LST_set - 24.0
     end if

     rise_set%set%A =  A_set
     rise_set%set%LST = LST_set

   end function rise_set

   !====================================================
   != Sec 46 - Sun Position
   != rise_set
   != Input :
   !====================================================
   function sun_pos(date_in)

     type(cDate) :: date_in
     type(ecliptic) :: sun_pos
     real :: currentDay, startYear,  v_prime, v
     real(kind=8) :: N, M, E, M_rad

     integer :: dayDiff, D
     real :: geo_ec_long, geo_ec_lat

!print *, "- debug - date_in ",date_in


     !Geocentric ecliptic latitude
     geo_ec_lat = 0


     !Number of days since the begining of the year
     currentDay = julian_date(date_in%day, date_in%month, date_in%year)


     startYear = julian_date(0.0, 1, date_in%year)
     dayDiff = int(currentDay - startYear )

!print *, "- debug - dayDiff ",dayDiff

!print *, "- debug - currentDay ",currentDay


     !Days since 1 Jan 1990
     D = int(julian_date(date_in%day, date_in%month, date_in%year) -  julian_date(0.0, 1, 1990))

!print *, "- debug - D1 ",julian_date(date_in%day, date_in%month, date_in%year)
!print *, "- debug - D2 ",julian_date(0.0, 1, 1990)

print *, "- debug - D ",D

     !Calculate N
     N = (360.0 * D)/ 365.242191
print *, "- debug - N` ",N

     if(N > 360.0) then
       do while (N > 360.0)
         N = N - 360.0
       end do
     else if (N < 0) then
       do while (N < 0.0)
         N = N + 360.0
       end do
     end if
print *, "- debug - N ",N




     !Find M
     M = N + s_ec_lon_epoch - s_ec_lon_perigee
     if (M < 0) then
       M = M + 360
     end if

     M_rad = deg_to_rad8(M)

     E = (360/Pi)*e_orb*sin(M_rad)

v = E + M

print *, "- debug - E ",E
print *, "- debug - M ",M
print *,"v", v
     geo_ec_long = N + E + s_ec_lon_epoch

print *, "geo_ec_long", geo_ec_long
     if (geo_ec_long > 360) then
       geo_ec_long = geo_ec_long -360
     end if

     sun_pos%la = geo_ec_lat
     sun_pos%lon = geo_ec_long

   end function



!##########################################################
!# Support Functions
!##########################################################

   !====================================================
   != degree_to_hours
   != Convert decimal degrees to decimal hours
   !====================================================
   function degree_to_hours(degree_in)
     real :: degree_to_hours, degree_in
     degree_to_hours = degree_in / 15
   end function degree_to_hours

   !====================================================
   != hours_to_degrees
   != Convert decimal hours to decimal degrees
   !====================================================
   function hours_to_degrees(hours_in)
     real :: hours_to_degrees, hours_in
     hours_to_degrees = hours_in * 15
   end function hours_to_degrees




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
   != dis_time
   !====================================================
   function dis_time(time_in)

     type(astrotime) :: time_in
     character(len=10) :: dis_time
     character(len=20) :: shr
     character(len=20) :: smin
     character(len=20) :: ssec



     write(shr, *) time_in%hr
     write(smin, *) time_in%min
     write(ssec, *) time_in%sec

     dis_time =adjustl(trim(shr)// ":" // adjustl(trim(smin)// ":" // adjustl(trim(ssec))))


   end function dis_time


   !====================================================
   != deg_to_rad
   !====================================================
   function deg_to_rad(deg_in)

      real :: deg_in, deg_to_rad

      deg_to_rad = deg_in  * Pi/180

   end function

  !====================================================
   != deg_to_rad8
   !====================================================
   function deg_to_rad8(deg_in)

      real(kind=8) :: deg_in, deg_to_rad8

      deg_to_rad8 = deg_in  * Pi/180

   end function

   !====================================================
   != rad_to_deg
   !====================================================
   function rad_to_deg(rad_in)

      real :: rad_in, rad_to_deg

      rad_to_deg = rad_in  * 180/Pi

   end function

   !====================================================
   != deg_dec_to_formal
   !====================================================
   function deg_dec_to_formal(deg_in)
     type(formal_deg) :: deg_dec_to_formal
     real :: deg_in
     integer :: degree, minute
     real :: second, min_prime

    degree = INT(deg_in)
    min_prime = deg_in -  degree
    minute = INT(min_prime * 60)
    second = (min_prime * 60) - minute
    second = second * 60


    deg_dec_to_formal%degree = degree
    deg_dec_to_formal%minute = minute
    deg_dec_to_formal%second = second

   end function

   !====================================================
   != dis_formal_deg
   !====================================================
   function dis_formal_deg(deg_in)

     type(formal_deg) :: deg_in
     character(len=25) :: dis_formal_deg
     character(len=20) :: sdegree
     character(len=20) :: sminute
     character(len=20) :: ssecond

     write(sdegree, *) deg_in%degree
     write(sminute, *) deg_in%minute
     write(ssecond, *) deg_in%second

     dis_formal_deg =adjustl(trim(sdegree)// "-deg " // adjustl(trim(sminute)// "' " // adjustl(trim(ssecond) // "'' ")))


   end function

   !====================================================
   != atan_cor_quad - Arc Tangent with Quadrant Correction
   !====================================================
   function atan_cor_quad(y, x)

     real :: atan_cor_quad
     real :: y,x,new_coor

     new_coor = atan(x/y)
     !determine quadrant
     if(x >= 0 .AND. y >= 0) then         ! First Quadrant
        !no action

     else if (x >= 0 .AND. y < 0) then    ! Second Quadrant
        new_coor = new_coor + Pi

     else if (x < 0 .AND. y < 0) then     ! Third Quadrant- not sure
        new_coor = new_coor + Pi

     else if (x < 0 .AND. y >= 0) then    ! Fourth Quadrant
        new_coor = (2 * Pi) + new_coor

     end if

     !Return value
     atan_cor_quad = new_coor

   end function


   !====================================================
   != kepler
   !====================================================
   function kepler(E_rad)
     real :: E_rad, E_rad_0, E_rad_1, E_delta
     real :: kepler

     real :: M
     real :: dec !declination

     logical :: continue = .FALSE.

     M = E_rad
     E_rad_0 = E_rad

     do while (continue .eqv. .FALSE.)
       dec = E_rad_0 - e_orb * sin(E_rad_0)  - M

         E_delta = (e_ec_rad) / (1 - e_orb * sin(E_rad_0))
         E_rad_1 = E_rad_0 - E_delta
E_rad_0 = E_rad_1

       if (ABS(dec) <= e_ec_rad) then
         continue = .TRUE.
       end if

       if (ABS(dec) > e_ec_rad) then
         E_delta = (e_ec_rad) / (1 - e_orb * sin(E_rad_0))
         E_rad_1 = E_rad_0 - E_delta
         E_rad_0 = E_rad_1
       end if

     end do

     kepler = E_rad_0

   end function


   !====================================================
   != print_coor -
   !====================================================
   subroutine print_coor(coor)
      type(coordinates) :: coor

      print *, "Ecliptic (", rad_to_deg(coor%ec%la), ",",rad_to_deg(coor%ec%lon), ")"
      print *, "Horizon (", rad_to_deg(coor%hz%az), "," , rad_to_deg(coor%hz%al), ")"
      print *, "Equaltorial (", rad_to_deg(coor%eq%ra), "," ,rad_to_deg(coor%eq%dec), ")"
      print *, "Galatic (", rad_to_deg(coor%ga%la), "," , rad_to_deg(coor%ga%lon), ")"


   end subroutine print_coor

   !====================================================
   != print_rise_set -
   !====================================================
   subroutine print_rise_set(data_in)
     type(body_rise_set) :: data_in

     print *,"Rise : ", dis_formal_deg(deg_dec_to_formal(rad_to_deg(data_in%rise%A))),dis_time(dtos(data_in%rise%LST))
     print *,"Set  : ", dis_formal_deg(deg_dec_to_formal(rad_to_deg(data_in%set%A))),dis_time(dtos(data_in%set%LST))

   end subroutine print_rise_set





end module modAstroCalc

!//////////////////////////////////////////////////////////////////////////////
program testMod

use modAstroCalc
implicit none

  real:: testJD , testSTOD, timein, d_out
  type(cDate) :: currentDate
  type(astrotime) :: currentTime
  type(coordinates) :: coor

  type(equatorial) :: coor_1, coor_2

  real :: coor_x, coor_y, coor_st, geo_lat
  character(2) :: coor_type

   type(body_rise_set) :: ers

  type(ecliptic) :: curr_sun_pos




print *,"=========================================="
   testJD = julian_date(17.25,2,1985)
  print *, testJD
  print *, day_of_week(testJD)

print *,"=========================================="
   currentDate = cal_date(2446113.75)
  print *, dis_cDate(currentDate)

print *,"=========================================="

  testSTOD = stod(6,31,27.4595)
  print *, testSTOD

print *,"=========================================="

  timein = 6.5242939
  currentTime =  dtos(timein)
  print *, "dtos :  ", dis_time(currentTime)

print *,"=========================================="

  testJD = UT_to_GST(2444351.5, 14.614353)
  print *, testJD

  currentTime =  dtos(testJD)
  print *, "dtos :  ", dis_time(currentTime)


print *,"=========================================="
  testJD = GST_to_UT(2444351.5, 4.668119)
  print *, testJD

 currentTime =  dtos(testJD)
  print *, "dtos :  ", dis_time(currentTime)

print *,"=========================================="

  testJD = LST(4.668119, -64.0)

  print *, testJD

  currentTime =  dtos(testJD)
  print *, "dtos :  ", dis_time(currentTime)

print *,"=========================================="

  testJD = LST_to_GST(0.401453, -64.0)
  print *, testJD

  currentTime =  dtos(testJD)
  print *, "dtos :  ", dis_time(currentTime)


print *,"=========================================="



  coor_st = 5.155862
  geo_lat = deg_to_rad(52.175278)

  ! ec -> hz
  coor_x  = deg_to_rad(97.638119)
  coor_y  = deg_to_rad(-17.857969)
  coor_type = "ec"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *,"ec -> hz"
  call print_coor(coor)

  !eq -> hz
  coor_x  = deg_to_rad(87.933333)
  coor_y  = deg_to_rad(23.219444)
  geo_lat = deg_to_rad(52.0)
  coor_type = "eq"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"eq -> hz"
  call print_coor(coor)

  ! hz -> eq
  coor_x  = deg_to_rad(283.271111)
  coor_y  = deg_to_rad(19.334444)
  geo_lat = deg_to_rad(52.0)
  coor_type = "hz"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"hz -> eq"
  call print_coor(coor)

  ! ec -> eq
  coor_x  = deg_to_rad(139.686111)
  coor_y  = deg_to_rad(4.875278)
  geo_lat = deg_to_rad(52.0)
  coor_type = "ec"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"ec -> eq"
  call print_coor(coor)

  ! eq -> ec
  coor_x  = deg_to_rad(143.723333)
  coor_y  = deg_to_rad(19.537278)
  geo_lat = deg_to_rad(52.0)
  coor_type = "eq"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"eq -> ec"
  call print_coor(coor)

  ! eq -> ga
  coor_x  = deg_to_rad(155.250000)
  coor_y  = deg_to_rad(10.350000)
  geo_lat = deg_to_rad(52.0)
  coor_type = "eq"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"eq -> ga"
  call print_coor(coor)

  ! ga -> eq
  coor_x  = deg_to_rad(232.247778)
  coor_y  = deg_to_rad(51.122222)
  geo_lat = deg_to_rad(52.0)
  coor_type = "ga"
  coor = coor_conv(coor_x, coor_y, coor_st, geo_lat, coor_type)
  print *, ""
  print *,"ga -> eq"
  call print_coor(coor)


print *, ""
print *,"=========================================="
  print *, "HA_TO_RA : ", HA_TO_RA(0.401453,5.862222)
  print *, "RA_TO_HA : ", RA_TO_HA(0.401453,18.539167)

print *, ""
print *,"=========================================="
  coor_1%ra   = deg_to_rad(hours_to_degrees(5.225472))
  coor_1%dec  = deg_to_rad(-8.225000)
  coor_2%ra   = deg_to_rad(hours_to_degrees(6.737056))
  coor_2%dec  = deg_to_rad(-16.686389)
  d_out = ang_btn_obj(coor_1, coor_2)

  print *, "ang_btn_obj : ", rad_to_deg(d_out)


print *, ""
print *,"=========================================="
print *, "Rise/Set"
coor_1%ra  = deg_to_rad(hours_to_degrees(23.655556))
coor_1%dec = deg_to_rad(21.700000)
geo_lat = deg_to_rad(30.0)

ers = rise_set(coor_1,geo_lat)
call print_rise_set(ers)

print *, ""
print *,"=========================================="
print *, "Sun Position"
currentDate%day = 27
currentDate%month = 7
currentDate%year = 1988

  curr_sun_pos = sun_pos(currentDate)
  coor_type = "ec"

  coor = coor_conv(deg_to_rad(curr_sun_pos%lon), deg_to_rad(curr_sun_pos%la), coor_st, geo_lat, coor_type)
  print *, ""
  print *,"Sun pos"
!  call print_coor(coor)


print *," Right Accension = ", dtos(degree_to_hours(rad_to_deg(coor%eq%ra) ))
print *," Declination     = ", deg_dec_to_formal(rad_to_deg(coor%eq%dec))


end program testMod


