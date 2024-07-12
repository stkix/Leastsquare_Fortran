program leastsquare
    implicit none
    integer :: i, n, v, c
    real(kind=8), allocatable :: x(:), y(:), u(:), w(:)  
    real(kind=8) :: sx, sy, sxx, sxy, syy
    real(kind=8) :: a, b
    real(kind=8) :: denom
 

   
    open(10, file='inp.txt')
    open(11, file='line.eps', action='write', status='replace')

    read(10, *) n
    allocate(x(n), y(n))

    do i = 1, n
        read(10, *) x(i), y(i)
    end do

    sx = sum(x)
    sy = sum(y)
    sxx = sum(x**2)
    sxy = sum(x*y)
    syy = sum(y**2)
    denom = real(n)*sxx - sx*sx
    a = (real(n)*sxy - sx*sy) / denom
    b = (sxx*sy - sxy*sx) / denom

    print '(a,es15.8,/,a,es15.8)', 'a=', a, 'b=', b

    v = 30

    allocate(u(v), w(v))
    u(1) = 0
    w(1) = b
    do c = 1, v
        u(c+1) = u(c) + 5
        w(c+1) = a*u(c+1) + b
    end do

    
    write(11, '(a)') '%!PS-Adobe-3.1 EPSF-3.0'
    write(11, '(a)') '%%BoundingBox: 0 0 500 800'
    write(11, '(a)') '%%EndComments'
    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 20 moveto'
    write(11, '(a)') '20 790 lineto'
    write(11, '(a)') 'stroke'

    !Thicken the line
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20.1 20.0 moveto'
    write(11, '(a)') '20.1 790 lineto'
    write(11, '(a)') 'stroke'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '20.2 20.0 moveto'
    write(11, '(a)') '20.2 790 lineto'
    write(11, '(a)') 'stroke'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '20.5 20.0 moveto'
    write(11, '(a)') '20.5 790 lineto'
    write(11, '(a)') 'stroke'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '21 20.0 moveto'
    write(11, '(a)') '21 790 lineto'
    write(11, '(a)') 'stroke'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 20 moveto'
    write(11, '(a)') '490 20 lineto'
    write(11, '(a)') 'stroke'

    !Thicken the line
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 20.1 moveto'
    write(11, '(a)') '490 20.1 lineto'
    write(11, '(a)') 'stroke'
    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 20.2 moveto'
    write(11, '(a)') '490 20.2 lineto'
    write(11, '(a)') 'stroke'
    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 20.5 moveto'
    write(11, '(a)') '490 20.5 lineto'
    write(11, '(a)') 'stroke'
    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '20 21 moveto'
    write(11, '(a)') '490 21 lineto'
    write(11, '(a)') 'stroke'



    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '15.5 770 moveto'
    write(11, '(a)') '20.5 790 lineto'
    write(11, '(a)') '25.5 770 lineto'
    write(11, '(a)') 'stroke'

    
    write(11, '(a)') 'newpath'
    write(11, '(a)') '470 25.5 moveto'
    write(11, '(a)') '490 20.5 lineto'
    write(11, '(a)') '470 15.5 lineto'
    write(11, '(a)') 'stroke'

    do i = 1, n
        write(11, '(a)') 'newpath'
        write(11, '(f6.2,x,f6.2,x,a)') x(i)+20, y(i)+20, '1 0 360 arc'
        write(11, '(a)') 'stroke'
    end do

    write(11, '(a)') 'newpath'
    write(11, '(f6.2,x,f6.2,x,a)') u(1)+20, w(1)+20, 'moveto'
    do c = 2, 30
        write(11, '(f6.2,x,f6.2,x,a)') u(c)+20, w(c)+20, 'lineto'
    end do

    write(11, '(a)') 'stroke'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '/Times-Roman findfont 10 scalefont setfont'
    write(11, '(a)') '9 9 moveto'
    write(11, '(a)') '(O) show'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '/Times-Roman findfont 20 scalefont setfont'
    write(11, '(a)') '480 27 moveto'
    write(11, '(a)') '(x) show'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '/Times-Roman findfont 20 scalefont setfont'
    write(11, '(a)') '27 770 moveto'
    write(11, '(a)') '(y) show'

    write(11, '(a)') 'newpath'
    write(11, '(a)') '/Times-Roman findfont 15 scalefont setfont'
    write(11, '(a)') '200 700 moveto'
    write(11, '(a,f15.8,a,f15.8,a)') '(y=',a,'x+(',b,')) show' 

    write(11, '(a)') 'showpage'
    close(11)


    deallocate(x, y, u, w)

end program leastsquare