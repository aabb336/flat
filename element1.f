      program element1
      implicit none
      real Xi,Yi,Xj,Yj,Xk,Yk,XB,YB,Pi,Pj,Pk,PB
      real ai,aj,ak,bi,bj,bk,ci,cj,ck,Ni,Nj,Nk,A2
      write(*,*)"Input Xi,Yi"
      read(*,*)Xi,Yi
      write(*,*)"Input Xj,Yj"
      read(*,*)Xj,Yj
      write(*,*)"Input Xk,Yk"
      read(*,*)Xk,Yk
      write(*,*)"Input Pi"
      read(*,*)Pi
      write(*,*)"Input Pj"
      read(*,*)Pj
      write(*,*)"Input Pk"
      read(*,*)Pk
2000  write(*,*)"Input XB,YB"
      read(*,*)XB,YB
      ai=Xj*Yk-Xk*Yj
      bi=Yj-Yk
      ci=Xk-Xj
      aj=Xk*Yi-Xj*Yi
      bj=Yk-Yi
      cj=Xi-Xk
      ak=Xi*Yj-Xj*Yi
      bk=Yi-Yj
      ck=Xj-Xi
      A2=Xj*Yk+Xi*Yj+Yi*Xk-Yi*Xj-Yj*Xk-Xi*Yk
      Ni=(ai+bi*XB+ci*YB)/A2
      Nj=(aj+bj*XB+cj*YB)/A2
      Nk=(ak+bk*XB+ck*YB)/A2
      PB=Ni*Pi+Nj*Pj+Nk*Pk
      write(*,1000)PB
1000  format (1x,"PB=",f7.2,"N/cm2")
      goto 2000
      pause
      end
