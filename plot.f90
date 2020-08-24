! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

program main
  use plotutil

  implicit none

  character(colmax) :: s, sc
  integer :: i, n, tn, fn

  real(rk), allocatable :: x(:)
  real(rk), allocatable :: y(:)

  character, allocatable :: code(:) ! plotting script
  real(rk), allocatable :: xdata(:) ! value data
  real(rk), allocatable :: ydata(:) ! time data

  real(rk), allocatable :: tx(:) ! theory
  real(rk), allocatable :: ty(:) ! time for theory
  
  real(rk), allocatable :: fx(:) ! fit
  real(rk), allocatable :: fy(:) ! time for fit
  real(rk), allocatable :: fymax(:)
  real(rk), allocatable :: fymin(:)

  real(rk), allocatable :: e(:), en(:) ! error

  real(rk) :: a, b, sa, sb, se, sigma

  call get_command_argument(1, sc)

  call get_command_argument(2, s)

  code = readfile(sc)

  xdata = parser(s, 1)
  ydata = parser(s, 2)
  e = parser(s, 3)
  
  !call sort(x, y, n)

  n = size(xdata)

  y = ydata!(1:100)
  !y = log(ydata)
  !x = 1.0_8/(xdata**2)!(1:100)
  x = xdata

  !call fitline(x, y, n, a, b)
  !call fla(e, n, 1._8)

  !e = sqrt(ydata + bg)

  !e = (log((ydata - bg) + e) - log((ydata - bg) - e)) / 2.0
  !e = sqrt((1.0/((ydata - bg)**2)) * (ydata + bg))
  !e = 1.0_8 / (sqrt(ydata + bg))

  !call fitlinesigmai(x, y, n, e, a, b, sa, sb)
  call fitlinesigmaimc(x, y, n, e, a, b, sa, sb, 32.0_8, 0.3_8)

  !sigma = log(sqrt(ydata(1) + bg))
  !sigma = sqrt(chisigma2(x, y, n, a, b))
  !call fitlinesigma(x, y, n, sigma, a, b, sa, sb)

  call mka(fx, 0.0_8, 18.0_8, 0.2_8)
  fy = a + fx * b
  fymax = (a+sa) + fx * (b+sb)
  fymin = (a-sa) + fx * (b-sb)

  !call mka(tx, 0.0_8, 17.0_8, 0.2_8)
  !ty = (ydata(1)) * exp(-0.12106 * tx)
  !ty = log(ty)

  write(0, *) "sigma: ", sigma
  write(0, *) "a: ", a, "b: ", b, "sa: ", sqrt(sa), "sb: ", sqrt(sb)
  !write(0, *) "avg err", sum(e)/real(n, rk)

  !write(0, *) "counts: ", sum(ydata)

  fn = size(fx)
  tn = size(tx)

  print *, code

  !call fla(en, tn, se)
  !call fla(e, n, sigma)

   call plotoute(x, y, e, n)
  !call plotout(x, y, n)

  call plotout(fx, fy, fn)
  call plotout(fx, fymax, fn)
  call plotout(fx, fymin, fn)
  
  !call plotout(tx, ty, tn)
contains
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
