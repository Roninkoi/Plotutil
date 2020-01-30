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
  
  real(rk), allocatable :: e0(:) ! error

  ! consts
  real(rk) :: t0 = 1
  real(rk) :: v0 = 1

  real(rk) :: a, b

  call get_command_argument(1, sc)
  
  call get_command_argument(2, s)

  code = readfile(sc)

  xdata = parser(s, 1)

  ydata = parser(s, 2) ! read data from file in command line arg
  
  !call sort(x, y, n)

  y = log(ydata)!(1:100)

  !x = 1.0_8/(xdata**2)!(1:100)
  x = xdata

  n = size(x);

  call fitline(x, y, n, a, b)

  call mka(tx, 0.003_8, 0.13_8, 0.001_8)
  ty = a + tx * b

  !x = sqrt(1.0_8 / (x))
  !tx = sqrt(1.0_8 / (tx))

  tn = size(tx)

  print *, code

  call plotout(x, y, n)

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
       !print *, x(i), y(i)

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
