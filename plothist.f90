! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

program main
  use plotutil

  implicit none

  character(colmax) :: s, sc
  integer :: i, n, tn

  real(rk) :: hmin, hmax, hdiff
  integer :: hi, hj, hn, hc

  real(rk), allocatable :: x(:)
  real(rk), allocatable :: y(:)

  character, allocatable :: code(:) ! plotting script
  real(rk), allocatable :: xdata(:) ! value data
  real(rk), allocatable :: ydata(:) ! time data

  real(rk), allocatable :: vxdata(:)
  real(rk), allocatable :: vydata(:)

  real(rk), allocatable :: tx(:) ! theory
  real(rk), allocatable :: ty(:) ! time for theory
  real(rk), allocatable :: tymax(:)
  real(rk), allocatable :: tymin(:)

  real(rk), allocatable :: e(:), en(:) ! error

  real(rk) :: a, b, sa, sb, se, sigma

  call get_command_argument(1, sc)

  call get_command_argument(2, s)

  code = readfile(sc)

  vxdata = parse_movie(s, 4, "10000", "100000")
  vydata = parse_movie(s, 5, "10000", "100000")
  xdata = sqrt(vxdata**2 + vydata**2) * 1.e5
  xdata = xdata * xdata * 3.29304e-7

  !call sort(x, y, n)

  n = size(xdata)

  hmin = minval(xdata)
  hmax = maxval(xdata)
  hn = 100
  hdiff = (hmax - hmin) / real(hn, rk)
  call mka(x, hmin, hmax, hdiff)
  call mka(y, hmin, hmax, hdiff)
  
  hi = 1
  do while (hi <= hn)
     hc = 0
     hj = 1
     do while (hj <= n)
        if (xdata(hj) <= x(hi) + hdiff .and. xdata(hj) >= x(hi)) then
           hc = hc + 1
        end if
        hj = hj + 1
     end do
     y(hi) = real(hc, rk)
     hi = hi + 1
  end do

  tn = hn * 10
  call mka(tx, hmin, hmax, hdiff / (tn / hn))
  ty = mbd(tx, tn, real(471, rk)) * 2436. * 20.

  write(0, *) "min: ", hmin, "max: ", hmax, "diff: ", hdiff
  write(0, *) "particles: ", n

  print *, code

  call plotout(x, y, hn)
  !call plotout(tx, ty, tn)
contains
  function mbd(x, n, a)
    integer :: n
    real(rk), intent(in) :: x(n)
    real(rk) mbd(n)
    real(rk) :: a

    mbd = sqrt(2. / pi) * (x**2 * exp(-x**2 / (2*a**2))) / (a**3)
  end function mbd
  
  function f(x, n)
    integer :: n
    real(rk), intent(in) :: x(n)
    real(rk) f(n)

    f = 0.049258_8*x**(2.103559_8)
  end function f

  subroutine plotout(x, y, n)
    real(rk), allocatable :: x(:)
    real(rk), allocatable :: y(:)
    character(16) :: so
    integer :: i, n

    i = 1
    do while(i <= n)
       write(*, '(f0.8, x, f0.8)') x(i), y(i)

       i = i + 1
    end do

    print '(a)', 'e'
  end subroutine plotout

  subroutine plotoute(x, y, e, n)
    real(rk), allocatable :: x(:)
    real(rk), allocatable :: y(:)
    real(rk), allocatable :: e(:)
    character(16) :: so
    integer :: i, n

    i = 1
    do while(i <= n)
       write(*, '(f0.8, x, f0.8, x, f0.8)') x(i), y(i), e(i)

       i = i + 1
    end do

    print '(a)', 'e'
  end subroutine plotoute
end program main
