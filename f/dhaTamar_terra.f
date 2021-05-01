      program dhatamar_terra
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

!	 EINGABEWERTE
!	 typ:	Schaltete um zwischen ATZ und NGZ
!	 ngz:	Eingabe Jahr
!	 atz:	Eingabe Jahr
!	 mona:	Eingabe Monat
!	 tag:	Eingabe Tag
!	 std:	Eingabe Stunde
!	 min:	Eingabe Minute
!	 sec:	Eingabe Sekunde

!	 ZWISCHENWERTE UND TESTWERTE
!        s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
!	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
!	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
!	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
!	 feb:	fuegt in Schaltjahren den 29.02. ein
!	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
!	 rueck: Regelt Neustart oder Abbruch des Programms
!        time:  
!        dif:   
!        v0:    5450 Periode dha-tamar
!        v1:    50 Jahre-Periode dha-tamar
!        AUSGABEWERTE
!        

      double precision  j0,j1,j2,j3,mon,t,tag,s,std,m,min,sec,s0,s1,s2
      double precision  s3,feb,rest,zeit,time,dif,nega,mem,dha
      double precision	v0,v1,vr0,vr1,vr2,co1,co2,co3,c1,c2,c3,ty
      integer           mona,monat,day,atz,ngz,ber,tor,rueck,typ,tamar
      integer           torlon
 
      parameter	(j0    = 1.0098216d11,
     .           j1    = 1.26227808d10,
     .		 j2    = 1.262304d8,
     .		 j3    = 3.1536d7,
     .		 t     = 8.64d4,
     .		 s     = 3.6d3,
     .		 m     = 6.d1,
     .           v0    = 1.71985248d11,
     .           v1    = 1.5778368d9,
     .           c1    = 7.2d3,
     .           c2    = 6.d2,
     .           c3    = 5.d1,
     .		 dif   = 1.779780211d12)
 
! Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.
      print *, 'Programm zur Umrechnung von dha-Tamar nach ATZ/NGZ'
      print *, 'V 1.0 beta (28.12.2002 by Christian Dalhoff)'
      print *, 'Bitte alle Eingaben mit "Return" bestaetigen!'
01    print *, 'Zuordnung der Torlon-Nummern:'
      print *, 'Jannhis  =  1'
      print *, 'Keub     =  2'
      print *, 'Nazhach  =  3'
      print *, 'Uhs      =  4'
      print *, 'Fohlad   =  5'
      print *, 'Sikkhla  =  6'
      print *, 'Adomet   =  7'
      print *, 'Aizhidos =  8'
      print *, 'Illhach  =  9'
      print *, 'Thiodege = 10'
      print *, 'Ezrah    = 11'
      print *, 'Eizhel   = 12'
      print *, 'Berlen`ty der Vrehetatou = 13'	
      print *, 'Geben sie das Jahr dha-Tamar ein:'
	read (*, *), dha
        if (dha == 0) then
	  print *, 'Ein Jahr 0 dha-Tamar existiert nicht!'
	  goto 01
        endif
        if (dha /= int(dha)) then
	  print *, 'Jahreseingaben muessen ganzzahlig sein!'
	  goto 01
        endif
02    print *, 'Geben sie den torlon ein (1-13):'
      read (*, *), torlon
      if (torlon /= int(torlon)) then
	 print *, 'Torlon muessen ganzzahlig sein!'
	 goto 02
      endif
      if ((torlon > 13) .or. (torlon < 1)) then
         print *, 'Torlon muessen im Bereich 1-13 liegen'
         goto 02
      endif
      if (torlon == 13) then
        if (dha > 0) then
          if ((int(dha/50)) /= (dha/50)) then
	      print *, 'Dieses Jahr dha ist kein Schaltjahr!'
	    goto 02
          endif
        endif
        if (dha < 0) then
          dha = dha+1
          if ((int(dha/50)) /= (dha/50)) then
	    print *, 'Im negativen Bereich wird -1, -51, ...geschaltet!'
	    goto 02
          endif
          dha = dha-1
        endif
      endif
 03   print *, 'Geben sie den ty ein (1-36):'
      read (*, *), ty
      if (ty /= int(ty)) then
	 print *, 'Ty muessen ganzzahlig sein!'
	 goto 03
      endif
      if ((ty > 36) .or. (ty < 1)) then
	print *, 'Ty muessen im Bereich 1-36 liegen'
	goto 03
      endif
      if (ty > 24) then
	 select case (torlon)
	   case (2, 4, 6, 8, 10)
	   print *, 'Dieser Torlon hat nur 24 Ty'
	   goto 03
	 end select
      endif
      if ((torlon == 11) .and. (ty > 33)) then
	 print *, 'Der Ezrah hat nur 33 Ty'
	 goto 03
      endif
      if ((torlon == 12) .and. (ty > 32)) then
	 print *, 'Der Eizhel hat nur 32 Ty'
	 goto 03
      endif
      if (torlon == 13) then
        if (dha > 0) then
          if ((int(dha/5450) /= (dha/5450)) .and. (ty>12)) then
           print *, 'Dieser torlon hat nur alle 5450 Jahre 24 Ty'
           goto 02
          endif
          if (ty > 12) then
            if ((int(dha/5450) == (dha/5450)) .and. (ty < 25)) then
              goto 04
            endif
           print *, 'Dieser Torlon hat nur 12 Ty'
           print *, '(Alle 5450 Jahre sind 24 Ty erlaubt)'
           goto 03
         endif
        endif
      endif
04    print *, 'Geben sie die Coberlen`ty ein (0-11):'
      read (*, *), co1
      if ((co1 > 11) .or. (co1 < 0)) then
	 print *, 'Coberlen`ty muessen im Bereich 0-11 liegen'
	 goto 04
      endif
      if (co1 /= int(co1)) then
	 print *, 'Coberlen`ty muessen ganzzahlig sein!'
	 goto 04
      endif
05    print *, 'Geben sie die Cobol`ty ein (0-11):'
      read (*, *), co2
      if ((co2 > 11) .or. (co2 < 0)) then
	 print *, 'Cobol`ty muessen im Bereich 0-11 liegen!'
	 goto 05
      endif
      if (co2 /= int(co2)) then
	 print *, 'Cobol`ty muessen ganzzahlig sein!'
	 goto 05
      endif
06    print *, 'Geben sie die Corgon`ty ein (0-11.99):'
      read (*, *), co3
      if ((co3 >= 12) .or. (co3 < 0)) then
	 print *, 'Sekunden muessen im Bereich 0-11.99 liegen!'
	 goto 06
      endif
!     Die folgende case-Anweisung ordnet dem Torlon die Zahl der seit
!     Jahresbeginn bis zum letzten des Vortorlon vergangenen Sekunden zu.
      select case (torlon)
	 case (1)
	 tor = 0
	 case(2)
	 tor = (36*t)
         case (3)
	 tor = (60*t)
	 case (4)
	 tor = (96*t)
	 case (5)
	 tor = (120*t)
	 case (6)
	 tor = (156*t)
	 case (7)
	 tor = (180*t)
	 case (8)
	 tor = (216*t)
	 case (9)
	 tor = (240*t)
	 case (10)
	 tor = (276*t)
	 case (11)
	 tor = (300*t)
	 case (12)
	 tor = (333*t)
	 case (13)
	 tor = (365*t)
      end select

! Berechnungen fuer positive Jahreszahlen dha-Tamar.
      if (dha > 0) then
! Berechnet die Anzahl der kompletten 5450-Jahre Schaltzyklen.
         s0=int((dha-1)/5450)
! Berechnet die Anzahl der kompletten 50-Jahre Schaltzyklen.
         s1=int((dha-1-s0*5450)/50)
! Berechnet die Anzahl der kompletten Jahre seit dem letzten Schaltjahr 
         s2=dha-s0*5450-s1*50-1
! Berechnet die Anzahl der Sekunden seit 1 dha-Tamar.
         zeit=s0*v0+s1*v1+s2*j3+tor+t*(ty-1)+co1*c1+co2*c2+co3*c3
      endif
! Fuehrt o.g. Operationen auf Jahreszahlen dha-Tamar kleiner 1 aus.
      if (dha == -1) then
! Berechnet die Anzahl der Sekunden bis 1 ATZ.
        zeit=-(j3+24*t-tor-(ty-1)*t-co1*c1-co2*c2-co3*c3)
      endif
      if (dha < -1) then
        dha=abs(dha)
        s0=int((dha-2)/5450)
        rest=dha-s0*5450
        s1=int((rest-2)/50)
	s2=dha-5450*s0-s1*50-1
! Berechnet die Anzahl der Sekunden bis 1 ATZ.
      zeit=-(s0*v0+s1*v1+s2*j3+24*t+j3-tor-(ty-1)*t
     .     -co1*c1-co2*c2-co3*c3)
      endif
! Kleine Vorsichtsmassnahme
       co1 = 0
       co2 = 0
       co3 = 0
       dha = 0
       torlon = 0
       ty  = 0
       ber = 0
       s0  = 0
       s1  = 0
       s2  = 0
       s3  = 0
       rest= 0
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
          goto 01
        endif
10    end
