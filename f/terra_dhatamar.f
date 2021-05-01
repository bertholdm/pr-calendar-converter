      program terra_dhatamar
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
!        time:  Sekunden bis 1 dha-Tamar
!        dif:   Differenz 1 ATZ bis 1 dha-Tamar in Sekunden
!        v0:    5450 Periode dha-tamar
!        v1:    50 Jahre-Periode dha-tamar
!        AUSGABEWERTE
!        

      double precision  j0,j1,j2,j3,mon,t,tag,s,std,m,min,sec,s0,s1,s2
      double precision  s3,feb,rest,zeit,time,dif,nega,mem
      double precision	v0,v1,vr0,vr1,vr2,co1,co2,co3,c1,c2,c3,ty
      integer           mona,monat,day,atz,ngz,ber,tor,rueck,typ,tamar
 
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
      print *, 'Programm zur Umrechnung von ATZ/NGZ nach dha-Tamar'
      print *, 'V 0.9 beta (28.12.2002 by Christian Dalhoff)'
      print *, 'Achtung Fehler in Schaltjahren im negativen dha-Bereich'
      print *, 'Bitte alle Eingaben mit "Return" bestaetigen!'
01    print *, 'Waehlen sie das Eingabeformat (ATZ = 0, NGZ = 1)'
      read (*, *), typ
      if ((typ /= int(typ)) .or. (typ >= 2) .or. (typ < 0)) then
	 print *, 'Kein zulaessiges Datumsformat!'
	 goto 01
      endif
      if (typ == 0) then
02	print *, 'Geben sie das Jahr ATZ ein:'
	read (*, *), atz
        if (atz == 0) then
	  print *, 'Ein Jahr 0 ATZ existiert nicht!'
	  goto 02
        endif
        if (atz /= int(atz)) then
	  print *, 'Jahreseingaben muessen ganzzahlig sein!'
	  goto 02
        endif
      endif
      if (typ == 1) then
03	    print *, 'Geben sie das Jahr NGZ ein:'
	    read (*, *), ngz
        if (ngz == 0) then
	  print *, 'Ein Jahr 0 NGZ existiert nicht!'
	  goto 03
        endif
        if (ngz /= int(ngz)) then
	  print *, 'Jahreseingaben muessen ganzzahlig sein!'
	  goto 03
        endif
! Umrechnung von NGZ in ATZ, die if Schleifen sind noetig, da beide
! Systeme kein Jahr 0 kennen!
        if (ngz >= 1) then
          atz = ngz+3587
        endif
        if (ngz < 1) then
          atz = ngz+3588
        endif
      endif
04    print *, 'Geben sie den Monat ein (1-12):'
      read (*, *), mona
      if (mona /= int(mona)) then
	 print *, 'Monatseingaben muessen ganzzahlig sein!'
	 goto 04
      endif
      if ((mona > 12) .or. (mona < 1)) then
         print *, 'Monate muessen im Bereich 1-12 liegen'
         goto 04
      endif
05    print *, 'Geben sie den Tag ein (1-31):'
      read (*, *), tag
      if (tag /= int(tag)) then
	 print *, 'Tageseingaben muessen ganzzahlig sein!'
	 goto 05
      endif
      if ((tag > 31) .or. (tag < 1)) then
	print *, 'Tage muessen im Bereich 1-31 liegen'
	goto 05
      endif
      if ((atz == 4) .and. (mona == 2))then
	 if (tag == 29) then
	  print *, 'Das Jahr 4 ATZ war kein Schaltjahr!'
	  goto 05
	 endif
      endif
      if ((mod((atz-1600), 3200) == 0) .and. (mona == 2)) then
	 if (tag == 29) then
	  print *, 'Dieses Jahr ATZ ist kein Schaltjahr!'
	  goto 05
	 endif
      endif    
      if (tag == 31) then
	 select case (mona)
	 case (4, 6, 9, 11)
	 print *, 'Dieser Monat hat nur 30 Tage!'
	 goto 05
	 endselect
      endif
06    print *, 'Geben sie die Stunde ein (0-23):'
      read (*, *), std
      if ((std > 23) .or. (std < 0)) then
	 print *, 'Stunden muessen im Bereich 0-23 liegen'
	 goto 06
      endif
      if (std /= int(std)) then
	 print *, 'Stundeneingaben muessen ganzzahlig sein!'
	 goto 06
      endif
07    print *, 'Geben sie die Minute ein (0-59):'
      read (*, *), min
      if ((min > 59) .or. (min < 0)) then
	 print *, 'Minuten muessen im Bereich 0-59 liegen!'
	 goto 07
      endif
      if (min /= int(min)) then
	 print *, 'Minuteneingaben muessen ganzzahlig sein!'
	 goto 07
      endif
08    print *, 'Geben sie die Sekunde ein (0-59):'
      read (*, *), sec
      if ((sec > 59) .or. (sec < 0)) then
	 print *, 'Sekunden muessen im Bereich 0-59 liegen!'
	 goto 08
      endif
      if (sec /= int(sec)) then
	 print *, 'Sekundeneingaben muessen ganzzahlig sein!'
	 goto 08
      endif
!     Die folgende case-Anweisung ordnet dem Monat die Zahl der seit
!     Jahresbeginn bis zum letzten des Vormonats vergangenen Sekunden zu.
      select case (mona)
	 case (1)
	 mon = 0
	 case(2)
	 mon = 2.6784d6
         case (3)
	 mon = 5.0976d6
	 case (4)
	 mon = 7.776d6
	 case (5)
	 mon = 1.0368d7
	 case (6)
	 mon = 1.30464d7
	 case (7)
	 mon = 1.56384d7
	 case (8)
	 mon = 1.83168d7
	 case (9)
	 mon = 2.09952d7
	 case (10)
	 mon = 2.35872d7
	 case (11)
	 mon = 2.62656d7
	 case (12)
	 mon = 2.88576d7
      end select
! Kleine Vorsichtsmassnahme
       s0 = 0
       s1 = 0
       s2 = 0
       s3 = 0
       rest = 0
       feb  = 0
! Berechnungen fuer positive Jahreszahlen ATZ.
      if (atz > 0) then
         s0=int((atz-1601)/3200)
         s1=int((atz-s0*3200-1)/400)
         rest   =atz-s0*3200-s1*400-1
        if (atz < 5) then
           s3=atz-1
        endif   
        if ((atz >= 5) .and. (atz < 1582)) then
! korrekte julianische Schaltung ab 8 ATZ bis zur gregorianischen
! Kalenderreform
	   s2=int((atz-1)/4)-1
! beruecksichtigt den ausgefallenen Schalttag 4 ATZ
	   s3=(atz-1)-(s2*4+4)
! Fuegt den 29. Februar  ein, wenn das aktuelle Jahr ein Schaltjahr ist.
	  if ((s3 == 3) .and. (mona > 2)) then
	   feb=t
	  endif
	  if ((s3 /= 3) .and. (mona == 2)) then
	     if (tag > 28) then
	        print *, 'In diesem Jahr hat der Februar nur 28 Tage!'
	        goto 01
	     endif
	  endif
          s3=s3+4
        endif
! Ab 1582 ATZ
        if (atz > 1582) then
	  s2=int(rest/4)
	  s3=rest-(s2*4)
	  if ((s3 == 3) .and. (mona > 2)) then
	     feb=t
	  endif
	  if ((s3 /= 3) .and. (mona == 2)) then
	     if (tag > 28) then
	        print *, 'In diesem Jahr hat der Februar nur 28 Tage!'
	        goto 01
	     endif
	  endif
	  if (mod(atz, 100) == 0) then
	     feb=0
	  endif
	  if ((mod(atz, 400) == 0) .and. (mona > 2)) then
	     feb=t
	  endif
	  if (mod((atz-1600), 3200) == 0) then
	     feb=0
	  endif
        endif
! Berechnet die Anzahl der Sekunden seit 1 ATZ.
      zeit=s0*j0+s1*j1+s2*j2+s3*j3+mon+t*(tag-1)+std*s+min*m+sec+feb
        if (atz >1582) then
           zeit=zeit+2*t
        endif
! Schleife fuer das Korrekturjahr 1582 ATZ.
        if (atz == 1582) then
          if (mona <= 9) then
        zeit=4.98924576d10+mon+(t*(tag-1))+(std*s)+(min*m)+sec
          endif
          if ((mona == 10) .and. (tag <= 4)) then
            zeit=4.99160448d10+(t*(tag-1))+(std*s)+(min*m)+sec
          endif
          if ((mona == 10) .and. (tag > 4)) then
            if (tag <= 14) then
	      print *, 'Der 5.-14. Oktober 1582 existieren nicht!'
              goto 01
            endif
            if (tag >= 15) then
	      s2=int(rest/4)
	      s3=rest-(s2*4)
          zeit=s1*j1+s2*j2+s3*j3+mon+(t*(tag-1)+(std*s)+(min*m)+sec+2*t)
             endif
         endif
         if (mona > 10) then
	   s2=int(rest/4)
	   s3=rest-(s2*4)
          zeit=s1*j1+s2*j2+s3*j3+mon+(t*(tag-1))+(std*s)+(min*m)+sec+2*t
         endif
        endif
      endif
! Fuehrt o.g. Operationen auf Jahreszahlen ATZ kleiner 1 aus.
      if (atz < 1) then
!Prueft ob der 29.Februar zulaessig ist:
      if ((mona == 2) .and. (tag == 29) .and. (atz > -46)) then
        select case (atz)
        case (-42,-39,-36,-33,-30,-27,-24,-21,-18,-15,-12,-9)
             goto 09
        case default
          print *, 'Dieses Jahr hatte keinen 29. Februar!'
          goto 01
        end select
      endif
09    if (atz >= -8) then
          atz=abs(atz)
      endif
      if ((atz <= -9) .and. (atz >= -45)) then
        atz=abs(atz)
        s2=(int((atz-1)/3))-2
        if (mona < 3) then
          select case (atz)
          case (42,39,36,33,30,27,24,21,18,15,12,9)
          feb=t
          end select
        endif
      endif
      if (atz < -45) then
        atz=abs(atz)
! Prueft ob der 29. Februar zulaessig ist!
        if ((tag == 29) .and. (mona == 2)) then
         if ((mod(atz+3, 100) == 0) .and. (mod((atz+3), 400) /= 0)) then
            print *, 'Dieses Jahr hatte keinen 29. Februar!'
            goto 01
         endif
          if ((atz+3)/4 /= int((atz+3)/4)) then
            print *, 'Dieses Jahr hatte keinen 29. Februar!'
            goto 01
          endif
        endif
        s0=int((atz-2)/3200)
        rest=atz-s0*3200
        s1=int((rest-2)/400)
	rest=atz-s0*3200-s1*400
	s2=int((rest-2)/4)
	s3=rest-(s2*4)-1
	if ((s3 == 4) .and. (mona < 3)) then
	   feb=t
	endif
        if ((s3 == 4) .and. (mona >= 3)) then
           feb=0
        endif
	if (s3 /= 4) then
	   feb=0
	endif
	if (mod(atz+3, 100) == 0) then
	   feb=0
	endif
	if ((mod((atz+3), 400) == 0) .and. (mona < 3)) then
	   feb=t
	endif
	if (mod(atz+3, 3200) == 0) then
	   feb=0
	endif
      endif
!      endif
! Berechnet die Anzahl der Sekunden bis 1 ATZ.
       zeit=-(s0*j0+s1*j1+s2*j2+s3*j3+feb+t+(j3-(mon+(tag-1)
     .      *t+std*s+min*m+sec)))
      endif
! Kleine Vorsichtsmassnahme
       co1 = 0
       co2 = 0
       co3 = 0
       ty  = 0
       ber = 0
       s0  = 0
       s1  = 0
       s2  = 0
       s3  = 0
       rest= 0
! Rechnet um von Sekunden seit 1 Atz auf Sekunden seit 1 da Ark
      time = zeit+dif
      if (time >= 0) then
! Berechnet die vollstaendigen 5450 Jahre-Perioden
        vr0=int(time/v0)
! Berechnet die vollstaendigen 50 Jahre-Perioden
        vr1=int((time-vr0*v0)/v1)
         if ((vr1/=0) .and. ((vr1/109) == (int(vr1/109)))) then
            vr1=vr1-1
         endif
! Berechnet die vollstaendigen Jahre
        vr2=int((time-vr0*v0-vr1*v1)/j3)
        if ((vr2 /= 0) .and. ((vr2/50) == (int((vr2/50))))) then
           vr2=vr2-1
        endif
        mem=time-vr0*v0-vr1*v1-vr2*j3
! Bestimmung des torlon (Monat) und ty (Tag)
         if (mem>2.592d7) then
           ty=int((mem-2.592d7)/t)
            if (ty<=32) then
               tor=11
            endif
            if ((ty>32) .and. (ty<=64)) then
               tor=12
               co1=int((mem-2.592d7-ty*t)/c1)
               co2=int((mem-2.592d7-ty*t-c1*co1)/c2)
               co3=(mem-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-33
            endif
            if (ty>64) then
               tor=13
               co1=int((mem-2.592d7-ty*t)/c1)
               co2=int((mem-2.592d7-ty*t-c1*co1)/c2)
               co3=(mem-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-65
            endif
         endif
         if (mem<=2.592d7) then
            ber=int(mem/(12*t))
            select case (ber)
               case (0,1,2)
                  tor=1
                  ty=int(mem/t)
                  co1=int((mem-ty*t)/c1)
                  co2=int((mem-ty*t-c1*co1)/c2)
                  co3=(mem-ty*t-c1*co1-c2*co2)/c3
               case (3,4)
                  tor=2
                  ty=int((mem-36*t)/t)
                  co1=int((mem-(ty+36)*t)/c1)
                  co2=int((mem-(ty+36)*t-c1*co1)/c2)
                  co3=(mem-(ty+36)*t-c1*co1-c2*co2)/c3
               case (5,6,7)
                  tor=3
                  ty=int((mem-60*t)/t)
                  co1=int((mem-(ty+60)*t)/c1)
                  co2=int((mem-(ty+60)*t-c1*co1)/c2)
                  co3=(mem-(ty+60)*t-c1*co1-c2*co2)/c3
               case (8,9)
                  tor=4
                  ty=int((mem-96*t)/t)
                  co1=int((mem-(ty+96)*t)/c1)
                  co2=int((mem-(ty+96)*t-c1*co1)/c2)
                  co3=(mem-(ty+96)*t-c1*co1-c2*co2)/c3
               case (10,11,12)
                  tor=5
                  ty=int((mem-120*t)/t)
                  co1=int((mem-(ty+120)*t)/c1)
                  co2=int((mem-(ty+120)*t-c1*co1)/c2)
                  co3=(mem-(ty+120)*t-c1*co1-c2*co2)/c3
               case (13,14)
                  tor=6
                  ty=int((mem-156*t)/t)
                  co1=int((mem-(ty+156)*t)/c1)
                  co2=int((mem-(ty+156)*t-c1*co1)/c2)
                  co3=(mem-(ty+156)*t-c1*co1-c2*co2)/c3
               case (15,16,17)
                  tor=7
                  ty=int((mem-180*t)/t)
                  co1=int((mem-(ty+180)*t)/c1)
                  co2=int((mem-(ty+180)*t-c1*co1)/c2)
                  co3=(mem-(ty+180)*t-c1*co1-c2*co2)/c3
               case (18,19)
                  tor=8
                  ty=int((mem-216*t)/t)
                  co1=int((mem-(ty+216)*t)/c1)
                  co2=int((mem-(ty+216)*t-c1*co1)/c2)
                  co3=(mem-(ty+216)*t-c1*co1-c2*co2)/c3
               case (20,21,22)
                  tor=9
                  ty=int((mem-240*t)/t)
                  co1=int((mem-(ty+240)*t)/c1)
                  co2=int((mem-(ty+240)*t-c1*co1)/c2)
                  co3=(mem-(ty+240)*t-c1*co1-c2*co2)/c3
               case (23,24)
                  tor=10
                  ty=int((mem-276*t)/t)
                  co1=int((mem-(ty+276)*t)/c1)
                  co2=int((mem-(ty+276)*t-c1*co1)/c2)
                  co3=(mem-(ty+276)*t-c1*co1-c2*co2)/c3
            end select
         endif
      endif
! Berechnet die Jahreszahl dha-Tamar
      tamar=vr0*5450+vr1*50+vr2+1
! Berechnet Daten kleiner 1 dha-Tamar
      if ((time < 0) .and. (time >= (-(j3+24*t)))) then
         tamar = -1
	 nega=j3+24*t-(abs(time))
         if (nega>2.592d7) then
           ty=int((nega-2.592d7)/t)
            if (ty<=32) then
               tor=11
            endif
            if ((ty>32) .and. (ty<=64)) then
               tor=12
               co1=int((nega-2.592d7-ty*t)/c1)
               co2=int((nega-2.592d7-ty*t-c1*co1)/c2)
               co3=(nega-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-33
            endif
            if (ty>64) then
               tor=13
               co1=int((nega-2.592d7-ty*t)/c1)
               co2=int((nega-2.592d7-ty*t-c1*co1)/c2)
               co3=(nega-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-65
            endif
         endif
         if (nega<=2.592d7) then
            ber=int(nega/(12*t))
            select case (ber)
               case (0,1,2)
                  tor=1
                  ty=int(nega/t)
                  co1=int((nega-ty*t)/c1)
                  co2=int((nega-ty*t-c1*co1)/c2)
                  co3=(nega-ty*t-c1*co1-c2*co2)/c3
               case (3,4)
                  tor=2
                  ty=int((nega-36*t)/t)
                  co1=int((nega-(ty+36)*t)/c1)
                  co2=int((nega-(ty+36)*t-c1*co1)/c2)
                  co3=(nega-(ty+36)*t-c1*co1-c2*co2)/c3
               case (5,6,7)
                  tor=3
                  ty=int((nega-60*t)/t)
                  co1=int((nega-(ty+60)*t)/c1)
                  co2=int((nega-(ty+60)*t-c1*co1)/c2)
                  co3=(nega-(ty+60)*t-c1*co1-c2*co2)/c3
               case (8,9)
                  tor=4
                  ty=int((nega-96*t)/t)
                  co1=int((nega-(ty+96)*t)/c1)
                  co2=int((nega-(ty+96)*t-c1*co1)/c2)
                  co3=(nega-(ty+96)*t-c1*co1-c2*co2)/c3
               case (10,11,12)
                  tor=5
                  ty=int((nega-120*t)/t)
                  co1=int((nega-(ty+120)*t)/c1)
                  co2=int((nega-(ty+120)*t-c1*co1)/c2)
                  co3=(nega-(ty+120)*t-c1*co1-c2*co2)/c3
               case (13,14)
                  tor=6
                  ty=int((nega-156*t)/t)
                  co1=int((nega-(ty+156)*t)/c1)
                  co2=int((nega-(ty+156)*t-c1*co1)/c2)
                  co3=(nega-(ty+156)*t-c1*co1-c2*co2)/c3
               case (15,16,17)
                  tor=7
                  ty=int((nega-180*t)/t)
                  co1=int((nega-(ty+180)*t)/c1)
                  co2=int((nega-(ty+180)*t-c1*co1)/c2)
                  co3=(nega-(ty+180)*t-c1*co1-c2*co2)/c3
               case (18,19)
                  tor=8
                  ty=int((nega-216*t)/t)
                  co1=int((nega-(ty+216)*t)/c1)
                  co2=int((nega-(ty+216)*t-c1*co1)/c2)
                  co3=(nega-(ty+216)*t-c1*co1-c2*co2)/c3
               case (20,21,22)
                  tor=9
                  ty=int((nega-240*t)/t)
                  co1=int((nega-(ty+240)*t)/c1)
                  co2=int((nega-(ty+240)*t-c1*co1)/c2)
                  co3=(nega-(ty+240)*t-c1*co1-c2*co2)/c3
               case (23,24)
                  tor=10
                  ty=int((nega-276*t)/t)
                  co1=int((nega-(ty+276)*t)/c1)
                  co2=int((nega-(ty+276)*t-c1*co1)/c2)
                  co3=(nega-(ty+276)*t-c1*co1-c2*co2)/c3
            end select
        endif
      endif
      if (time < (-(j3+24*t))) then
        time = -(time+j3+24*t)
        vr0=int(time/v0)
        rest=(time-vr0*v0)
	vr1=int(rest/v1)
        rest=rest-vr1*v1
	vr2=int(rest/j3)
        tamar=-(vr0*5450+vr1*50+vr2+2)
	nega=j3-(rest-vr2*j3)
        if (nega>2.592d7) then
            ty=int((nega-2.592d7)/t)
            if (ty<=32) then
               tor=11
            endif
            if ((ty>32) .and. (ty<=64)) then
               tor=12
               co1=int((nega-2.592d7-ty*t)/c1)
               co2=int((nega-2.592d7-ty*t-c1*co1)/c2)
               co3=(nega-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-33
            endif
            if (ty>64) then
               tor=13
               co1=int((nega-2.592d7-ty*t)/c1)
               co2=int((nega-2.592d7-ty*t-c1*co1)/c2)
               co3=(nega-2.592d7-ty*t-c1*co1-c2*co2)/c3
               ty=ty-65
            endif
         endif
         if (nega<=2.592d7) then
            ber=int(nega/(12*t))
            select case (ber)
               case (0,1,2)
                  tor=1
                  ty=int(nega/t)
                  co1=int((nega-ty*t)/c1)
                  co2=int((nega-ty*t-c1*co1)/c2)
                  co3=(nega-ty*t-c1*co1-c2*co2)/c3
               case (3,4)
                  tor=2
                  ty=int((nega-36*t)/t)
                  co1=int((nega-(ty+36)*t)/c1)
                  co2=int((nega-(ty+36)*t-c1*co1)/c2)
                  co3=(nega-(ty+36)*t-c1*co1-c2*co2)/c3
               case (5,6,7)
                  tor=3
                  ty=int((nega-60*t)/t)
                  co1=int((nega-(ty+60)*t)/c1)
                  co2=int((nega-(ty+60)*t-c1*co1)/c2)
                  co3=(nega-(ty+60)*t-c1*co1-c2*co2)/c3
               case (8,9)
                  tor=4
                  ty=int((nega-96*t)/t)
                  co1=int((nega-(ty+96)*t)/c1)
                  co2=int((nega-(ty+96)*t-c1*co1)/c2)
                  co3=(nega-(ty+96)*t-c1*co1-c2*co2)/c3
               case (10,11,12)
                  tor=5
                  ty=int((nega-120*t)/t)
                  co1=int((nega-(ty+120)*t)/c1)
                  co2=int((nega-(ty+120)*t-c1*co1)/c2)
                  co3=(nega-(ty+120)*t-c1*co1-c2*co2)/c3
               case (13,14)
                  tor=6
                  ty=int((nega-156*t)/t)
                  co1=int((nega-(ty+156)*t)/c1)
                  co2=int((nega-(ty+156)*t-c1*co1)/c2)
                  co3=(nega-(ty+156)*t-c1*co1-c2*co2)/c3
               case (15,16,17)
                  tor=7
                  ty=int((nega-180*t)/t)
                  co1=int((nega-(ty+180)*t)/c1)
                  co2=int((nega-(ty+180)*t-c1*co1)/c2)
                  co3=(nega-(ty+180)*t-c1*co1-c2*co2)/c3
               case (18,19)
                  tor=8
                  ty=int((nega-216*t)/t)
                  co1=int((nega-(ty+216)*t)/c1)
                  co2=int((nega-(ty+216)*t-c1*co1)/c2)
                  co3=(nega-(ty+216)*t-c1*co1-c2*co2)/c3
               case (20,21,22)
                  tor=9
                  ty=int((nega-240*t)/t)
                  co1=int((nega-(ty+240)*t)/c1)
                  co2=int((nega-(ty+240)*t-c1*co1)/c2)
                  co3=(nega-(ty+240)*t-c1*co1-c2*co2)/c3
               case (23,24)
                  tor=10
                  ty=int((nega-276*t)/t)
                  co1=int((nega-(ty+276)*t)/c1)
                  co2=int((nega-(ty+276)*t-c1*co1)/c2)
                  co3=(nega-(ty+276)*t-c1*co1-c2*co2)/c3
            end select
         endif
      endif   
! Ausgabe der Ergebnisse   
      print *, ty+1, 'ty'
        select case (tor)
           case (1)
              print *, 'des Jannhis'
           case (2)
              print *, 'der Keub'
           case (3)
              print *, 'des Nazhach'
           case (4)
              print *, 'der Uhs'
           case (5)
              print *, 'des Fohlad'
           case (6)
              print *, 'der Sikkhla'
           case (7)
              print *, 'des Adomet'
           case (8)
              print *, 'der Aizhidos'
           case (9)
              print *, 'des Illhach'
           case (10)
              print *, 'der Thiodege'
           case (11)
              print *, 'des Ezrah'
           case (12)
              print *, 'der Eizhel'
           case (13)
              print *, 'berlen`ty dha vrehetatou'
           end select
       print *, tamar, ' dha-tamar'
       print *, co1, 'coberlen`ty', co2,'cobol`ty', co3, ' corgon`ty'
       if (tamar > 6500) then
          print *, 'Das Lemurische Tammanium gilt ab 6417 dha-Tamar als' 
          print *, 'Untergegangen, Lemuria ab 6500 als zerstoert!'
       endif
      print *, 'Eine weitere Umrechnung	durchfuehren 0 = nein 1 = ja?'
      read (*,*), rueck
      if (rueck	/= 1) then
        goto 11
        endif
      if (rueck	== 1) then
        goto 01
      endif
11    end
