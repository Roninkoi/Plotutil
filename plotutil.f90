! Data plotter and utilites in Fortran 90
! Uses Gnuplot
! Roni Koitermaa 2020
! github.com/Roninkoi/Plotutil

module plotutil
  implicit none

  integer, parameter :: rk = 8
  integer, parameter :: colmax = 3000
  real(rk) :: pi = 3.14159265359

  character(colmax) :: line

contains
  function boxmuller(n, m)
    real(4) :: n, m
    real(4) :: boxmuller

    boxmuller = sqrt(-2.0*log(n)) * cos(2.0*pi*m)
  end function boxmuller

  function normrand(mu, sigma)
    real(rk) :: mu, sigma
    real(rk) :: normrand

    normrand = boxmuller(rand(), rand()) * sigma + mu
  end function normrand

  function normaldist(mu, sigma, x)
    real(rk) :: mu, sigma, x
    real(rk) :: normaldist

    normaldist = 1.0/(sigma * sqrt(2.0*pi)) * exp(-((x-mu)/sigma)**2/2.0)
  end function normaldist

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

    sa = sqrt((sx2) / d)
    sb = sqrt((ss) / d)
  end subroutine fitlinesigmai

  subroutine fitlinesigmaimc(xdata, ydata, n, sigma, a, b, sa, sb, ag, bg)
    real(rk), allocatable :: xdata(:)
    real(rk), allocatable :: ydata(:)
    real(rk), allocatable :: sigma(:)

    integer :: n, i, ii, sn
    real(rk) :: a, b, sa, sb
    real(rk) :: am, bm, pm
    real(rk) :: sq, sql, ra, rb, ag, bg

    a = ag
    b = bg
    sa = 0.0
    sb = 0.0
    sql = 100000000.0

    sn = 1
    am = 0.0
    bm = 0.0

    do ii = 1, 50000000
       sq = 0.0

       !ra = a + (rand() - 0.5) * a * 0.0001
       !rb = b + (rand() - 0.5) * b * 0.0001
       ra = normrand(a, sqrt(sa))
       rb = normrand(b, sqrt(sb))

       i = 1
       do while (i <= n)
          sq = sq + (normrand(ydata(i), sigma(i)) - (ra + rb * xdata(i)))**2 / (sigma(i)**2)
          i = i + 1
       end do

       if (sq < sql) then
          a = ra
          b = rb

          sn = sn + 1

          pm = am
          am = am + (a - am) / sn
          sa = sa + ((a - pm) * (a - am) - sa) / sn

          pm = bm
          bm = bm + (b - bm) / sn
          sb = sb + ((b - pm) * (b - bm) - sb) / sn

          sql = sq
       end if
    end do

    sa = sqrt(sa)
    sb = sqrt(sb)
  end subroutine fitlinesigmaimc

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

    sa = sqrt((sx2) / d)
    sb = sqrt((ss) / d)
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

  character(len = colmax) function str(r)
    real :: r
    write(str, *) r
  end function str

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

       if (ch == ',' .or. ch == ' ' .or. ch == char(9) .or. ch == char(10)) then
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

    if (cch == '' .or. cc /= c) then
       cm = 1
       return
    endif

    read(cch, *) r
  end subroutine column

  ! separated by tabs or spaces, hash is comment
  function string_column(c) result(cch)
    integer :: c
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

       if (ch == ',' .or. ch == ' ' .or. ch == char(9) .or. ch == char(10)) then
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
    cch = trim(cch)
  end function string_column

  function parser(p, c) ! create array from file
    character(colmax) :: p
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

  function parse_movie(p, c, t_start, t_end) ! create array from file
    character(colmax) :: p
    integer :: c
    character(len=*) :: t_start, t_end
    real(rk), allocatable :: parse_movie(:)

    integer :: i, n, io, cm
    real(rk), allocatable :: a(:)
    real(rk) :: s

    logical :: reading

    open(8, file = p, action = 'read')

    allocate(a(0))

    reading = .false.

    n = 2
    i = 1
    do while (i < n .or. .true.)
       read(8, '(A)', iostat = io) line

       if (io /= 0) then
          exit
       endif

       if (reading .eqv. .false.) then
          !print *, trim(string_column(1)), "Time=" // trim(t_start)
          if (trim(string_column(1)) == "Time=" // trim(t_start)) then
             reading = .true.
          end if
          cycle
       end if

       if (trim(string_column(1)) == "Time=" // trim(t_end)) then
          exit
       end if

       if (line(1:5) == "Time=") then
          cycle
       end if
       
       !print *, trim(line)

       cm = 0
       call column(s, c, cm)
       !print *, cm

       if (cm == 0) then
          n = n + 1
          call append(a, s)
       end if
       i = i + 1
    end do

    allocate(parse_movie(n))

    close(8)

    !print *, a

    parse_movie = a

    deallocate(a)
  end function parse_movie

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

