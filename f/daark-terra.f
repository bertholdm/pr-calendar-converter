      program daark_terra
      implicit none

!	 ZEITEINHEITEN IN SEKUNDEN
!        j0:    3200*365.2421875 Tage in Sekunden 
!	 j1:	400*356,2425 Tage in Sekunden
!	 j2:	4*365,25 Tage in Sekunden
!	 j3:	365 Tage in Sekunden
!	 mon:	Dauer der Vormonate in Sekunden
!	 t:	ein Tag in Sekunden
!	 s:	eine Stunde in Sekunden
!	 m:	eine Minute in Sekunden
!        ark1:  ein Jahr da Ark in Sekunden (Variiert, daher a-c)
!        per:   eine Periode in Sekunden (Variiert, daher a-c)
!        pra:   ein Prago in Sekunden (Variiert, daher a-c)
!        ton:   eine Tonta in Sekunden (Variiert, daher a-c)

!	 EINGABEWERTE


!	 ZWISCHENWERTE UND TESTWERTE
!        s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
!	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
!	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
!	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
!	 feb:	fuegt in Schaltjahren den 29.02. ein
!	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
!	 rueck: Regelt Neustart oder Abbruch des Programms
!        time:  Anzahl der Sekunden seit 1. Prago des Eyilon 1 da Ark
!        dif:   da Ark in Sekunden am 1. Januar 1 ATZ
!        vre1:  Anzahl der Schaltperioden seit 1 da Ark
!        vre2:  Anzahl der Jahre im aktuellen Schaltzyklus da Ark
!        vre3:  Anzahl der 25 Jahre Schaltperioden innerhalb dieser
!               50 Jahre Periode
!        neg:   Zwischenwert bei da Ark Werten kleiner 0


!        AUSGABEWERTE
!	 ngz:	   Jahreszahl NGZ
!	 atz:	   Eingabe Jahr
!	 mona:	   Eingabe Monat
!	 tage:	   Eingabe Tag
!	 stun:	   Eingabe Stunde
!	 minu:	   Eingabe Minute
!	 sec:	   Eingabe Sekunde
    
      double precision  j0,j1,j2,j3,mon,t,m,sec,s,s0,s1,s2,s3,feb,rest 
      double precision  zeit,time,dif
      double precision	ark1,ark1a,ark1b,ark2,ark1c,ark2a,ark2b,ark2c
      double precision	per,per_a,per_b,per_c,peri,prago,pra,pra_a 
      double precision  pra_b,pra_c,ton,ton_a,ton_b,ton_c,tonta
      double precision  daark,vre1,vre2,vre3,nega
      integer           atz,ngz,mona,monat,tag,day,std,min,rueck,typ
      integer           periode

      parameter	(j0  = 1.0098216d11,
     .           j1  = 1.26227808d10,
     .		 j2  = 1.262304d8,
     .		 j3  = 3.1536d7,
     .		 t   = 8.64d4,
     .		 s   = 3.6d3,
     .		 m   = 6.d1,      
     .		 ark1  = 1.86477700824d9,
     .           ark1a = 1.86477335604d9,
     .           ark1b = 1.86478066044d9,
     .           ark1c = 1.86498323748d9,
     .		 ark2  = 3.72730742022d7,
     .           ark2a = 3.72730012022d7,
     .           ark2b = 3.72730815022d7,
     .           ark2c = 3.72731140383d7,
     .		 per   = 3.67624841447d6,
     .           per_a = 3.67624121447d6,
     .           per_b = 3.67625561447d6,
     .           per_c = 3.67625234351d6,
     .		 pra   = 1.02118011513d5,
     .           pra_a = 1.02117811513d5,
     .           pra_b = 1.02118211513d5,
     .           pra_c = 1.02118120653d5,
     .		 ton   = 5.10590057565d3,
     .           ton_a = 5.10589057565d3,
     .           ton_b = 5.10591057565d3,
     .           ton_c = 5.10590603263d3,
     .		 dif   = 6.44647557545d11)

! Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.
      print *, 'Programm zur Umrechnung von Daten da Ark nach ATZ/NGZ'
      print *, 'V 4.0 beta (30.09.2002 by Christian Dalhoff)'
      print *, 'Bitte alle Eingaben mit "Return" bestaetigen!'
 21   vre1 = 0
      vre2 = 0
      vre3 = 0
      daark = 0
      peri = 0
      prago = 0
      tonta = 0
      print *, 'Zuordnung der Perioden-Nummern:'
      print *, 'Eyilon =  1'
      print *, 'Hara   =  2'
      print *, 'Tarman =  3'
      print *, 'Dryhan =  4'
      print *, 'Messon =  5'
      print *, 'Tedar  =  6'
      print *, 'Ansoor =  7'
      print *, 'Prikur =  8'
      print *, 'Coroma =  9'
      print *, 'Tartor = 10'
      print *, 'Katanen des Capit   = 11'
      print *, 'Pragos des Vretatou = 12'
      print *, 'Tiga Ranton = 13'

      print *, 'Geben sie das Jahr da Ark ein:'
      read (*, *), daark
      if (daark == 0) then
         print *, 'Ein Jahr 0 da Ark existiert nicht!'
         goto 21
      endif
      if (daark /= int(daark)) then
         print *, 'da Ark-Eingaben muessen ganzzahlig sein!'
         goto 21
      endif
22    print *, 'Geben sie die Periode (1-13) ein:'
      read (*, *), periode
      if ((periode > 13) .or. (periode < 1)) then
         print *, 'Perioden muessen im Bereich 1-13 liegen!'
         goto 22
      endif
      if (periode /= int(periode)) then
         print *, 'Perioden muessen ganzzahlig sein!'
         goto 22
      endif
      if ((periode == 12) .and. ((daark/50-int(daark/50)) /= 0)) then
         print *, 'In diesem Jahr gab es keine Pragos des Vretatou!'
         goto 22
      endif
      if (periode == 12) then
         periode=11
         peri=5
      endif
      if ((periode == 13) .and. (daark < 19300)) then
         print *, 'Tiga Ranton wurde erst 19300 da Ark eingefuehrt!'
         goto 22
      endif
      if ((periode == 13) .and. (daark > 21499)) then
         print *, 'Tiga Ranton wurde 21500 da Ark abgeschafft!'
         goto 22
      endif
      if ((periode == 13) .and. ((daark/25-int(daark/25))/= 0)) then
         print *, 'In diesem Jahr gab es kein Tiga Ranton!'
         goto 22
      endif
      if (periode == 13) then
         periode=11
         peri=16
      endif
23    print *, 'Geben sie den Prago ein (1-36):'
      read (*, *), prago
      if ((prago > 36) .or. (prago < 1)) then
         print *, 'Pragos muessen im Bereich 1-36 liegen!'
         goto 23
      endif
      if (prago /= int(prago)) then
         print *, 'Pragos muessen ganzzahlig sein!'
         goto 23
      endif
      if (periode == 11)  then
         if ((prago > 5) .and. (peri==0)) then
         print *, 'Die Katanen des Capits haben maximal 5 Pragos!'
         goto 23
         endif
         if ((peri == 5) .and. (prago > 11)) then
         print *, 'Die Pragos des Vretatou haben maximal 11 Pragos!'
         goto 23
         endif
         if ((peri == 16) .and. (prago > 1)) then
         print *, 'Tiga Ranton ist nur ein Prago!'
         goto 23
         endif
      endif
24    print *, 'Geben sie die Tonta ein (0-19.9999):'
      read (*, *), tonta
      if ((tonta  > 20) .or. (tonta < 0)) then
         print *, 'Tontas muessen im Bereich 0-19.9999 liegen!'
         goto 24
      endif
! Berechnet fuer positive Jahreszahlen da Ark die seit 1 da Ark
! vergangenen kompletten Schaltperioden und die Anzahl der Jahre seit der
! Letzten.
      if (daark > 0) then
        vre1 = int((daark-1)/50)
        vre2 = (daark-1)-(vre1*50)
! Bis 6500 da Ark
        if (daark <= 6500) then
! Berechnet die Anzahl der Sekunden seit 1 da Ark.
        time = vre1*ark1+vre2*ark2+(periode-1)*per+peri*pra
     .         +(prago-1)*pra+tonta*ton
        endif
! 6501 da Ark bis 8750 da Ark
        if ((daark > 6500) .and. (daark <= 8750)) then
           vre1 = vre1-130
           time = 2.42421011071d11+vre1*ark1a+vre2*ark2a+(periode-1)
     .            *per_a+peri*pra_a+(prago-1)*pra_a+tonta*ton_a
        endif
! 8751 da Ark bis 14000 da Ark
        if ((daark > 8750) .and. (daark <= 14000)) then
           vre1 = vre1-175
           time = 3.26335812093d11+vre1*ark1+vre2*ark2+(periode-1)*per
     .            +peri*pra+(prago-1)*pra+tonta*ton
        endif
! 14001 da Ark bis 16250 da Ark
        if ((daark > 14000) .and. (daark <= 16250)) then
           vre1 = vre1-280
           time = 5.22137397958d11+vre1*ark1b+vre2*ark2b+(periode-1)
     .            *per_b+peri*pra_b+(prago-1)*pra_b+tonta*ton_b
        endif
! 16250 bis zur Zerstoerung von ArkonIII
        if ((daark >= 16251) .and. (daark < 19300)) then
        time = vre1*ark1+vre2*ark2+(periode-1)*per+peri*pra
     .         +(prago-1)*pra+tonta*ton
        endif
! Nach der Zerstoerung von ArkonIII 
      if (daark == 19300) then
        time = 7.19765528807d11+(periode-1)*per_c+peri*pra_c+(prago-1)
     .         *pra_c+tonta*ton_c
      endif
      if ((daark >= 19301) .and. (daark < 21500)) then
        vre3 = int((daark-19301)/25)
        vre2 = (daark-19301)-(vre3*25)
        if (((vre3/2-int(vre3/2)) == 0) .and. (peri == 16)) then
           peri = 5
        endif
        time =7.19804027313d11+vre2*ark2c+vre3*(25*ark2c+pra_c)+
     .        int(vre3/2)*11*pra_c+(periode-1)*per_c+(prago-1)*pra_c
     .        +tonta*ton_c+peri*pra_c
      endif
! Nach der Wiederherstellung von Tiga Ranton
      if (daark == 21500) then
        time = 8.0182479123038d11+(periode-1)*per+peri*pra+
     .         (prago-1)*pra+tonta*ton
      endif
      if (daark >= 21501) then
        vre1 = (daark-21501)/50
        vre2 = (daark-21501)-(vre1*50)
        time =8.01863187602d11+vre1*ark1+vre2*ark2+(periode-1)*per
     .        +(prago-1)*pra+tonta*ton+peri*pra
      endif
      endif
! Rechnet um in Sekunden seit 1 atz
      zeit = time-dif
! Kleine Vorsichtsmassnahme
      s0=0
      s1=0
      s2=0
      s3=0
      mon=0
      mona=0
      tag=0
! 1 ATZ bis 4 ATZ
       if ((zeit >= 0) .and. (zeit < 1.26143999d8)) then
         s3=int(zeit/j3)
         atz=s3+1
         mon=zeit-s3*j3
       endif
! 5 ATZ bis 4.10.1582 ATZ
       if ((zeit >= 1.26143999d8) .and. (zeit <= 4.99163904d10)) then
         s2=(int((zeit/(j2))))-1
         s3=int((zeit-(s2*j2))/j3)
         atz=s2*4+s3+1
         mon=zeit-(s2*j2+((s3)*j3))
         s3=s3-4
       endif
! Ab 15.10.1582 ATZ  
      if (zeit > 4.99163904d10) then
        zeit=zeit-2*t
        s0=int((zeit-5.04911232d10)/j0)
        s1=int((zeit-s0*j0)/j1)
        s2=int((zeit-s0*j0-s1*j1)/j2)
        s3=int((zeit-s0*j0-s1*j1-s2*j2)/j3)
        atz=s0*3200+s1*400+s2*4+s3+1
        mon=zeit-s0*j0-s1*j1-s2*j2-s3*j3
! Korrektur fuer nicht durch 4 teilbare Jahrhunderte
        if (mod(atz, 100) == 0) then
          s3=0
        endif
        if (mod(atz, 400) == 0) then
          s3=3
        endif
        if (mod((atz-1600), 3200) == 0) then
           s3=0
        endif
      endif
! Ordnet den Monat zu.
          if (mon <= 2.6784d6) then
             monat=1
             day=mon
          endif
          if (mon > 2.6784d6) then
             monat=2
             day=mon-2.6784d6 
          endif
          if ((mon > 5.0976d6) .and. (s3 /= 3)) then
             monat=3
             day=mon-5.0976d6
          endif
          if ((mon > 5.184d6) .and. (s3 == 3)) then
             monat=3
             day=mon-5.184d6
          endif
          if ((mon > 7.776d6) .and. (s3 /= 3)) then
             monat=4
             day=mon-7.776d6
          endif
          if ((mon > 7.8624d6) .and. (s3 == 3)) then
             monat=4
             day=mon-7.8624d6
          endif
          if ((mon > 1.0368d7) .and. (s3 /= 3)) then
             monat=5
             day=mon-1.0368d7
          endif
          if ((mon > 1.04544d7) .and. (s3 == 3)) then
             monat=5
             day=mon-1.04544d7
          endif
          if ((mon > 1.30464d7) .and. (s3 /= 3)) then
             monat=6
             day=mon-1.30464d7
          endif
          if ((mon > 1.31328d7) .and. (s3 == 3)) then
             monat=6
             day=mon-1.31328d7
          endif
          if ((mon > 1.56384d7) .and. (s3 /= 3)) then
             monat=7
             day=mon-1.56384d7
          endif
          if ((mon > 1.57248d7) .and. (s3 == 3)) then
             monat=7
             day=mon-1.57248d7
          endif
          if ((mon > 1.83168d7) .and. (s3 /= 3)) then
             monat=8
             day=mon-1.83168d7
          endif
          if ((mon > 1.84032d7) .and. (s3 == 3)) then
             monat=8
             day=mon-1.84032d7
          endif
          if ((mon > 2.09952d7) .and. (s3 /= 3)) then
             monat=9
             day=mon-2.09952d7
          endif
          if ((mon > 2.10816d7) .and. (s3 == 3)) then
             monat=9
             day=mon-2.10816d7
          endif
          if ((mon > 2.35872d7) .and. (s3 /= 3)) then
             monat=10
             day=mon-2.35872d7
          endif
          if ((mon > 2.36736d7) .and. (s3 == 3)) then
             monat=10
             day=mon-2.36736d7
          endif
          if ((mon > 2.62656d7) .and. (s3 /= 3)) then
             monat=11
             day=mon-2.62656d7
          endif
          if ((mon > 2.6352d7) .and. (s3 == 3)) then
             monat=11
             day=mon-2.6352d7
          endif
          if ((mon > 2.88576d7) .and. (s3 /= 3)) then
             monat=12
             day=mon-2.88576d7
          endif
          if ((mon > 2.8944d7) .and. (s3 == 3)) then
             monat=12
             day=mon-2.8944d7
          endif
! -1 ATZ bis -9 ATZ
      if (zeit < 0) then
      if ((zeit < 0) .and. (zeit > -2.52288d8)) then
        zeit=abs(zeit)
        s3=int(zeit/j3)
        atz=-(s3+1)
        mona=int((zeit-s3*j3))
        mon=j3-mona
        s3=0
      endif
! -9 ATZ bis -45 ATZ
      if ((zeit < -2.52288d8) .and. (zeit >= -1.4201568d9)) then
        zeit=abs(zeit)
        s2=int((zeit-6*j3)/(j3*3+t))
        s3=int(((zeit)-s2*(j3*3+t)-6*j3)/j3)
        atz=-(s2*3+s3+7)
        mona=(zeit-s2*(3*j3+t)-(s3+6)*j3)
        mon=j3-mona
        s3=s3-1 
! Beruecksichtigt, dass -45 ATZ kein Schaltjahr war
        if (atz == 45) then
          s3 = 0
        endif
      endif
! Bis -45 ATZ
      if (zeit < -1.4201568d9) then
! -t gleicht die Fehler zu Beginn der julianischen Schaltung aus!
        zeit=(abs(zeit))-t
        s0=int(zeit/j0)
        s1=int((zeit-s0*j0)/j1)
        s2=int((zeit-s0*j0-s1*j1)/j2)
        s3=int((zeit-s0*j0-s1*j1-s2*j2)/j3)
        atz=-(s0*3200+s1*400+s2*4+s3+1)
        mona=zeit-s0*j0-s1*j1-s2*j2-s3*j3
        mon=j3-mona
        s3=s3+1
! Korrektur fuer nicht durch 4 teilbare Jahrhunderte
        if (mod((atz+3), 100) == 0) then
          s3=0
        endif
        if (mod((atz+3), 400) == 0) then
          s3=1
        endif
        if (mod((atz+3), 3200) == 0) then
          s3=0
        endif
       endif
! Berechnung des Monats
          if (mon <= 2.6784d6) then
             monat=1
             day=mon
          endif
          if (mon > 2.6784d6) then
             monat=2
             day=mon-2.6784d6
          endif
          if ((mon > 5.0976d6) .and. (s3 /= 1)) then
             monat=3
             day=mon-5.0976d6
          endif
          if ((mon > 5.184d6) .and. (s3 == 1)) then
             monat=3
             day=mon-5.184d6
          endif
          if ((mon > 7.776d6) .and. (s3 /= 1)) then
             monat=4
              day=mon-7.776d6
          endif
          if ((mon > 7.8624d6) .and. (s3 == 1)) then
             monat=4
             day=mon-7.8624d6
          endif
          if ((mon > 1.0368d7) .and. (s3 /= 1)) then
             monat=5
             day=mon-1.0368d7
          endif
          if ((mon > 1.04544d7) .and. (s3 == 1)) then
             monat=5
             day=mon-1.04544d7
          endif
          if ((mon > 1.30464d7) .and. (s3 /= 1)) then
             monat=6
             day=mon-1.30464d7
          endif
          if ((mon > 1.31328d7) .and. (s3 == 1)) then
             monat=6
             day=mon-1.31328d7
          endif
          if ((mon > 1.56384d7) .and. (s3 /= 1)) then
             monat=7
             day=mon-1.56384d7
          endif
          if ((mon > 1.57248d7) .and. (s3 == 1)) then
             monat=7
             day=mon-1.57248d7
          endif
          if ((mon > 1.83168d7) .and. (s3 /= 1)) then
             monat=8
             day=mon-1.83168d7
          endif
          if ((mon > 1.84032d7) .and. (s3 == 1)) then
             monat=8
             day=mon-1.84032d7
          endif
          if ((mon > 2.09952d7) .and. (s3 /= 1)) then
             monat=9
             day=mon-2.09952d7
          endif
          if ((mon > 2.10816d7) .and. (s3 == 1)) then
             monat=9
             day=mon-2.10816d7
          endif
          if ((mon > 2.35872d7) .and. (s3 /= 1)) then
             monat=10
             day=mon-2.35872d7
          endif
          if ((mon > 2.36736d7) .and. (s3 == 1)) then
             monat=10
             day=mon-2.36736d7
          endif
          if ((mon > 2.62656d7) .and. (s3 /= 1)) then
             monat=11
             day=mon-2.62656d7
          endif
          if ((mon > 2.6352d7) .and. (s3 == 1)) then
             monat=11
             day=mon-2.6352d7
          endif
          if ((mon > 2.88576d7) .and. (s3 /= 1)) then
             monat=12
             day=mon-2.88576d7
          endif
          if ((mon > 2.8944d7) .and. (s3 == 1)) then
             monat=12
             day=mon-2.8944d7
          endif
        endif  
      tag=int(day/t)
      std=int((day-tag*t)/s)
      min=int((day-tag*t-std*s)/m)
      sec=int(day-tag*t-std*s-min*m)
        if (tag == 31) then
         select case (monat)
         case (1)
          monat=2
          tag=0
         case (3)
          monat=4
          tag=0        
         case (5)
          monat=6
          tag=0
         case (7)
          monat=8
          tag=0
	 case (8)
          monat=9
          tag=0
         case (10)
          monat=11
          tag=0
         case (12)
          monat=1
          tag=0
          if (atz > 0) then
           atz=atz-1
          endif
          if (atz < 0) then
           atz=atz+1
          endif
         endselect
        endif
        if (tag == 30) then
         select case (monat)
	 case (2)
          monat=3
          tag=0
	 case (4)
          monat=5
          tag=0
	 case (6)
          monat=7
          tag=0
	 case (9)
          monat=10
          tag=0
	 case (11)
          monat=12
          tag=0
         endselect
        endif
        if ((tag == 29) .and. (monat == 2)) then
         monat=3
         tag=0
        endif
        if (atz <= 3587) then
         ngz=atz-3588
        endif
        if (atz > 3587) then
         ngz=atz-3587
        endif
      print *, tag+1, '.', monat, '.', atz, ' ATZ', ' /', ngz, ' NGZ'
      print *, std, 'h', min, 'min', int(sec), 'sec'
      print *, 'Eine weitere Umrechnung durchfuehren 0 = nein 1 = ja?'
      read (*,*), rueck
        if (rueck /= 1) then
          goto 10
        endif
        if (rueck == 1) then
          goto 21
        endif
10    end



