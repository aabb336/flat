      program plate dist
      implicit none
      real E,h,mu,D,q,a,b,w,w1,w2,w0,w01,w02,D1
      real::h1=0
      integer::m=1,n=1
2000  write(*,*)"Input Elasticity modulus E, kN/cm2"
      read(*,*) E
      write(*,*)"Input Thickness h, cm"
      read(*,*)h
      write(*,*)"Input Poisson ratio"
      read(*,*)mu
      D=E*(h**3)/(12*(1-mu**2))
      write (*,1000)D
1000  format (1x,"D=",f8.2)
      write(*,*) "Input length of plate,cm"
      read (*,*)a
      write(*,*) "Input width of plate,cm"
      read (*,*)b
      write(*,*) "Input distributed load q, kN/m2"
      read (*,*)q
      w1=(16*q/(10000*958*D))
      w2=(-1)/(m*n*((m**2/a**2)+(n**2/b**2))**2)
      w=w1*w2
      write (*,1010)w
1010  format (1x,"Deflection w=",f8.3,1x,"cm")
      do
        h1=h1+0.2
        D1=E*(h1**3)/(12*(1-mu**2))
        w01=(16*q/(10000*958*D1))
        w02=(-1)/(m*n*((m**2/a**2)+(n**2/b**2))**2)
        w0=w01*w02
        write(*,1020)h1,w0
1020    format (1x,"h=",f8.3,1x,"cm",4x,"w=",f8.3,"cm")
        if (h1>=3)exit
        enddo
      goto 2000
      pause
      end
