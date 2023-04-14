C     Programa 2-1
C     (p. 114)
C Definicao da ordem de substituicao ou eliminacao num sistema de
C equacoes de coficientes tensoriais segundo uma estrategia tipo A
C
C Programa na versao original, com adaptacoes minimas do 
C Fortran 1 para Fortran 77
C
      dimension a(4,160),n(160),ni(40),no(40)
  999 read 1,m,na
      do 10 i=1,m
   10 ni(i)=0
      do 20 i=1,na
      read 2,n(i),a(1,i),a(2,i),a(3,i),a(4,i)
      k1=n(i)/100
   20 ni(k1)=ni(k1)+1
      do 30 i=1,m
      nimi=9998
      do 40 j=1,m
      if (nimi-ni(j))40,40,50
   50 nimi=ni(j)
      jj=j
   40 continue
      write (*,1) jj,i
      no(jj)=1
   30 ni(jj)=9999
      write(*,1) m,na
      do 60 i=1,na
      k1=n(i)/100
      k2=n(i)-100*k1
      n(i)=100*no(k1)+no(k2)
   60 write (*,2) n(i),a(1,i),a(2,i),a(3,i),a(4,i)
      go to 999
    1 format (2i5)
    2 format (i5,4e14.8)
      end

