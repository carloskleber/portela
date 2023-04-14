C     Programa 2-2
C     (p. 116-117)
C Definicao da ordem de substituicao ou eliminacao num sistema de
C equacoes de coficientes tensoriais segundo uma estrategia tipo C
C
C Programa na versao original, com adaptacoes minimas do 
C Fortran 1 para Fortran 77
C
      dimension n(300),nc(60),nl(60),nnn(200),ke(59)
  999 read 1,m,na
      nas=na
      do 10 i=1,na
   10 read 1,n(i)
      do 200 k=1,m
      nnm=9999
      ka=0
      do 20 kk=1,m 
      ken=k-1
      if(ken)150,150,110
  110 do 250 l=1,ken
      if (kk-ke(l))250,20,250
  250 continue
  150 j=0
      jj=0
      do 40 i=1,na 
      k1=n(i)/100
      k2=n(i)-100*k1
      if (k1-kk)50,60,50
   50 if (k2-kk)40,70,40
   70 jj=jj+1
      nl(jj)=k1
      go to 40
   60 if (k2-kk)80,40,80
   80 j=j+1
      nc(j)=k2
   40 continue
      i0=j*jj
      if (i0)140,140,170
  170 i0=0
      do 90 i=1,j
      do 90 ii=1,jj
      no=100*nl(ii)+nc(i)
      do 100 l=1,na
      if (n(l)-no)100,90,100
  100 continue
      i0=i0+1
      nnn(i0)=no
      if (i0-nnm)90,160,160
   90 continue
      if(i0)140,140,160
  160 if (ka)130,130,140
  130 if (i0-nnm)120,20,20
  120 kkm=kk
      nnm=i0
   20 continue
      ka=1
      kk=kkm
      go to 150
  140 nas=nas+i0
      write (*,1) kk,na,i0,nas
      ke(k)=kk
      naa=na
      do 190 i=1,na
  330 if (n(i)/100-kk)190,300,190
  300 naa=naa-1
      write (*,1) n(i),kk,kk
      n(i)=0
      if(i-naa)320,320,340
  320 do 310 l=i,naa
  310 n(l)=n(l+1)
      go to 330
  190 continue
  340 na=naa+i0
      if (i0)200,200,240
  240 do 210 i=1,i0
      j=naa+i
      n(j)=nnn(i)
      write (*,1) n(j),kk
  210 continue
  200 continue
      go to 999
    1 format (4i5)
      end
      