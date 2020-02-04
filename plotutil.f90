! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

module plotutil
  implicit none

  integer, parameter :: rk = 8
  integer, parameter :: colmax = 3000

  character(colmax) :: line

contains
  function chisigma2(xdata, ydata, n, a, b)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)
    real(rk) :: chisigma2

    integer :: n, i
    real(rk) :: a, b, sum

    sum = 0
    i = 1
    do while (i <= n)
       sum = sum + (ydata(i) - a - b * xdata(i))**2
       i = i + 1
    end do

    chisigma2 = (sum)/(n - 2)
  end function chisigma2
  
  subroutine fitlinesigmai(xdata, ydata, n, sigma, a, b, sa, sb)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)
    real(rk), allocatable :: sigma(:)

    integer :: n, i
    real(rk) :: a, b, sa, sb
    real(rk) :: d, sx2, sx, sy, sxy, ss, s2

    d = 0.0
    sx2 = 0.0
    sx = 0.0
    sy = 0.0
    sxy = 0.0
    ss = 0.0

    i = 1
    do while (i <= n)
       s2 = sigma(i)**2

       sx2 = sx2 + (xdata(i)**2)/s2

       ss = ss + 1.0/s2

       sx = sx + xdata(i)/s2
       sy = sy + ydata(i)/s2
       sxy = sxy + (xdata(i) * ydata(i))/s2

       i = i + 1
    end do

    d = ss*sx2 - sx**2

    a = (sx2 * sy - sx * sxy) / d
    b = (ss * sxy - sx * sy) / d

    sa = (sx2) / d
    sb = (ss) / d
  end subroutine fitlinesigmai

  subroutine fitlinesigma(xdata, ydata, n, sigma, a, b, sa, sb)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)

    integer :: n, i
    real(rk) :: a, b, sa, sb
    real(rk) :: d, sx2, sx, sy, sxy, ss, s2, sigma

    s2 = sigma**2

    d = 0.0
    sx2 = 0.0
    sx = 0.0
    sy = 0.0
    sxy = 0.0
    ss = 0.0

    i = 1
    do while (i <= n)
       sx2 = sx2 + (xdata(i)**2)/s2

       ss = ss + 1.0/s2

       sx = sx + xdata(i)/s2
       sy = sy + ydata(i)/s2
       sxy = sxy + (xdata(i) * ydata(i))/s2

       i = i + 1
    end do

    d = ss*sx2 - sx**2

    a = (sx2 * sy - sx * sxy) / d
    b = (ss * sxy - sx * sy) / d

    sa = (sx2) / d
    sb = (ss) / d
  end subroutine fitlinesigma

  subroutine fitline(xdata, ydata, n, a, b)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)

    integer :: n, i
    real(rk) :: a, b, d, sx2, sx, sy, sxy

    d = 0.0
    sx2 = 0.0
    sx = 0.0
    sy = 0.0
    sxy = 0.0

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

  subroutine fla(a, n, v) ! fill array
    real(rk), allocatable :: a(:)
    real(rk) :: v

    integer :: n, i

    allocate(a(n))

    do i = 1, n, 1
       a(i) = v
    end do
  end subroutine fla

  ! separated by tabs or spaces, hash is comment
  subroutine column(r, c, cm)
    integer :: c, cm
    real(rk) :: r

    character :: ch
    character(colmax) :: cch
    logical :: ws

    integer :: i, n, cc

    i = 1
    n = colmax
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

  subroutine appends(a, v) ! append string to array
    character, allocatable :: a(:)
    character(*) :: v

    character, allocatable :: b(:)
    integer :: n, m, i

    n = size(a)
    m = len(v)

    allocate(b(n))

    b = a
    deallocate(a)
    allocate(a(n + m))

    a(1:n) = b
    !a(n+1:n+m) = v(1:m)

    i = 1
    do while (i <= m)
       a(n+i) = v(i:i)
       i = i + 1
    end do
  end subroutine appends

  subroutine appendc(a, v) ! append char to array
    character, allocatable :: a(:)
    character :: v

    character, allocatable :: b(:)
    integer :: n

    n = size(a)

    allocate(b(n))

    b = a
    deallocate(a)
    allocate(a(n + 1))

    a(1:n) = b
    a(n + 1) = v
  end subroutine appendc

  function readfile(p)
    character(32) :: p
    character, allocatable :: readfile(:)

    integer :: i, io

    open(8, file = p, action = 'read')

    allocate(readfile(0))

    i = 1
    do while (.true.)
       read(8, '(A)', iostat = io) line

       if (io /= 0) then
          exit
       endif

       call appends(readfile, trim(line) // char(10))

       i = i + 1
    end do

    close(8)
  end function readfile

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

