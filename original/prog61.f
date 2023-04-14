C     Programa 6-1
C     (p. 222)
C Conversao de uma matriz de admitancias de elementos dispersos
C constituidos por tensores de segunda ordem na matriz T
C correspondente
C
C Programa na versao original, com adaptacoes minimas do 
C Fortran 1 para Fortran 77
C
      dimension u(2,200),d(2,200),a(2,2),t(2,2)
  999 read 1,m,nr1,nr2
      do 10 i=1,m
      read 2,i0,ua,ub
      k=i0-(i0/100)*100
      u(1,k)=ua
      u(2,k)=ub
      um=sqrt(ua**2+ub**2)
      d(1,k)=ua/um
   10 d(2,k)=ub/um
   50 read 1,m,na,mi
      write (*,1) m,na,mi
      do 20 i=1,na
      read 2,n,a(1,1),a(1,2),a(2,1),a(2,2)
      ii=n/100
      jj=n-100*ii
      do 30 k=1,2
      t(1,k)=u(1,ii)*a(1,k)+u(2,ii)*a(2,k)
   30 t(2,k)=u(2,ii)*a(1,k)-u(1,ii)*a(2,k)
      do 40 k=1,2
      a(k,1)=t(k,1)*d(1,jj)+t(k,2)*d(2,jj)
   40 a(k,2)=-t(k,1)*d(2,jj)+t(k,2)*d(1,jj)
   20 write (*,2) n,a(1,1),a(1,2),a(2,1),a(2,2)
C     if(sense switch 1)999,50
    1 format(3i5)
    2 format(i5,4e14.8)
      end