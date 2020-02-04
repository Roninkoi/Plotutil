! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

program main
  use plotutil

  implicit none

  character(80) :: s, sc
  integer :: i, n, tn

  real(rk), allocatable :: x(:)
  real(rk), allocatable :: y(:)

  character, allocatable :: code(:) ! plotting script
  real(rk), allocatable :: xdata(:) ! value data
  real(rk), allocatable :: ydata(:) ! time data

  real(rk), allocatable :: tx(:) ! theory
  real(rk), allocatable :: ty(:) ! time for theory
  real(rk), allocatable :: tymax(:)
  real(rk), allocatable :: tymin(:)

  real(rk), allocatable :: e(:), en(:) ! error

  real(rk) :: a, b, sa, sb, se, sigma

  call get_command_argument(1, sc)

  call get_command_argument(2, s)

  code = readfile(sc)

  xdata = parser(s, 1)
  ydata = parser(s, 2)

  !call sort(x, y, n)

  y = ydata!(1:100)
  x = 1.0_8/(xdata**2)!(1:100)
  !x = xdata

  n = size(x)

  call fitline(x, y, n, a, b)
  !call fla(e, n, 1._8)

  !call fitlinesigmai(x, y, n, e, a, b, sa, sb)

  sigma = sqrt(sum(y)/real(n,rk))
  !sigma = sqrt(chisigma2(x, y, n, a, b))
  call fitlinesigma(x, y, n, sigma, a, b, sa, sb)

  call mka(tx, 0.003_8, 0.16_8, 0.02_8)
  ty = a + tx * b
  tymax = (a+sqrt(sa)) + tx * (b+sqrt(sb))
  tymin = (a-sqrt(sa)) + tx * (b-sqrt(sb))

  se = sqrt(sa + sb)

  write(0, *) "sigma: ", sigma
  write(0, *) "a: ", a, "b: ", b, " as2: ", sa, " bs2: ", sb, " fit err: ", se

  tn = size(tx)

  print *, code

  !call fla(en, tn, se)
  call fla(e, n, sigma)

  call plotoute(x, y, e, n)

  call plotout(tx, ty, tn)
  call plotout(tx, tymax, tn)
  call plotout(tx, tymin, tn)
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
