! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

module plotutil
  implicit none

  integer, parameter :: rk = 8

  character(3000) :: line

contains
  subroutine fitline(xdata, ydata, n, a, b)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)

    integer :: n, i
    real(rk) :: a, b, d, sx2, sx, sy, sxy

    d = 0.0_8
    sx2 = 0.0_8
    sx = 0.0_8
    sy = 0.0_8
    sxy = 0.0_8

    i = 1
    do while (i <= n)
       sx2 = sx2 + xdata(i)**2

       sx = sx + xdata(i)
       sy = sy + ydata(i)
       sxy = sxy + xdata(i) * ydata(i)

       i = i + 1
    end do

    d = n*sx2 - sx**2

    a = (sx2 * sy - sx * sxy) / d
    b = (n * sxy - sx * sy) / d
  end subroutine fitline

  subroutine mka(a, min, max, diff) ! make array
    real(rk), allocatable :: a(:)
    real(rk) :: max, min, diff

    integer :: n, i

    n = (max - min)/diff

    allocate(a(n))

    a(1) = min
    do i = 2, n, 1
       a(i) = a(i - 1) + diff
    end do
  end subroutine mka

  ! separated by tabs or spaces, hash is comment
  subroutine column(r, c, cm)
    integer :: c, cm
    real(rk) :: r

    character :: ch
    character(3000) :: cch
    logical :: ws

    integer :: i, n, cc

    i = 1
    n = 3000
    cc = 0
    ws = .false.
    cch = ''

    do while (i < n)
       ch = line(i:i)

       if (ch == '#') then ! comment
          exit
       endif

       !print *, trim(ch), trim(cch)

       if (ch == ' ' .or. ch == char(9) .or. ch == char(10)) then
          ws = .true.
       else
          if (ws .eqv. .true.) then
             ws = .false.
             cc = cc + 1

             if (cc == c) then
                exit;
             else
                cch = ''
             end if
          end if

          cch = trim(cch) // ch
       end if

       i = i + 1
    end do

    if (cch == '') then
       cm = 1
       return
    endif

    read(cch, *) r
  end subroutine column

  function parser(p, c) ! create array from file
    character(32) :: p
    integer :: c
    real(rk), allocatable :: parser(:)

    integer :: i, n, io, cm
    real(rk), allocatable :: a(:)
    real(rk) :: s

    open(8, file = p, action = 'read')

    allocate(a(0))

    n = 2
    i = 1
    do while (i < n .or. .true.)
       read(8, '(A)', iostat = io) line

       if (io /= 0) then
          exit
       endif

       cm = 0
       call column(s, c, cm)
       !print *, cm

       if (cm == 0) then
          n = n + 1
          call append(a, s)
       end if
       i = i + 1
    end do

    allocate(parser(n))

    close(8)

    !print *, a

    parser = a

    deallocate(a)
  end function parser

  subroutine append(a, v) ! append real to array
    real(rk), allocatable :: a(:)
    real(rk) :: v

    real(rk), allocatable :: b(:)
    integer :: n

    n = size(a)

    allocate(b(n))

    b = a
    deallocate(a)
    allocate(a(n + 1))

    a(1:n) = b
    a(n + 1) = v
  end subroutine append

  subroutine sort(xdata, ydata, n) ! sort by x inc
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)

    integer :: n, i, j, mini

    real(rk) :: minx, tx, ty

    i = 1
    do while (i <= n)
       minx = xdata(i)
       mini = i

       j = i + 1
       do while (j <= n)
          tx = xdata(j)

          if (tx < minx) then
             minx = tx
             mini = j
          end if

          j = j + 1
       end do

       tx = xdata(i)
       ty = ydata(i)

       xdata(i) = xdata(mini)
       ydata(i) = ydata(mini)

       xdata(mini) = tx
       ydata(mini) = ty

       i = i + 1
    end do
  end subroutine sort
end module plotutil

program main
  use plotutil

  implicit none

  character(32) :: s
  integer :: i, n, tn

  real(rk), allocatable :: x(:)
  real(rk), allocatable :: y(:)

  real(rk), allocatable :: xdata(:) ! value data
  real(rk), allocatable :: ydata(:) ! time data

  real(rk), allocatable :: tx(:) ! theory
  real(rk), allocatable :: ty(:) ! time for theory
  
  real(rk), allocatable :: e0(:) ! error

  ! consts
  real(rk) :: t0 = 1
  real(rk) :: v0 = 1

  real(rk) :: a, b

  call get_command_argument(1, s)

  xdata = parser(s, 1)

  ydata = parser(s, 2) ! read data from file in command line arg
  
  !call sort(x, y, n)

  y = ydata!(1:100)
  !v = f(v, n)

  x = 1.0_8/(xdata**2)!(1:100)
  !x = xdata

  n = size(x);

  call fitline(x, y, n, a, b)

  call mka(tx, 0.0_8, 0.13_8, 0.001_8)
  ty = a + tx * b

  tn = size(tx)

  !print *, a, b

!  print '(a)', 'set datafile separator ","'
  print '(a)', 'set title "Säteily"'
  print '(a)', 'set xlabel "1/r2 [1/cm2]"'
  print '(a)', 'set ylabel "N [1/min]"'
  print '(a)', 'set grid'
  print '(a)', 'set key top left; set style data points'
  
  print '(a)', 'plot "-" using 1:2 title "data" with linespoints pt 7 lt 5 lc rgb "#ffaa00", ' // &
       '"-" using 1:2 title "fit" with lines lt 5 lc rgb "#ff0000"'

  ! using 1:2:3 with yerrorbars
  ! with linespoints pt 7 lt 5 lc rgb "#ffffff"

  call plotout(x, y, n)

  call plotout(tx, ty, tn)
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
       write(*, '(f0.8 x f0.8)') x(i), y(i)
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
       write(*, '(f0.8 x f0.8 x f0.8)') x(i), y(i), e(i)

       i = i + 1
    end do

    print '(a)', 'e'
  end subroutine plotoute
end program main
