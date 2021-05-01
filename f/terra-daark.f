      program terra_daark
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
!        time:  Anzahl der Sekunden seit 1. Prago des Eyilon 1 da Ark
!        dif:   da Ark in Sekunden am 1. Januar 1 ATZ
!        vre1:  Anzahl der Schaltperioden seit 1 da Ark
!        vre2:  Anzahl der Jahre im aktuellen Schaltzyklus da Ark
!        vre3:  Anzahl der 25 Jahre Schaltperioden innerhalb dieser
!               50 Jahre Periode
!        neg:   Zwischenwert bei da Ark Werten kleiner 0

!        AUSGABEWERTE
!        daark:    Jahreszahl da Ark
!        periode:  Periode da Ark
!        prago:    Prago da Ark
!        tonta:    Tontas da Ark

      double precision  j0,j1,j2,j3,mon,t,tag,s,std,m,min,sec,s0,s1,s2
      double precision  s3,feb,rest,zeit,time,dif,vre1,vre2,vre3,daark
      double precision	ark1,ark1a,ark1b,ark1c,ark2,ark2a,ark2b,ark2c
      double precision	per,per_a,per_b,per_c,peri,prago,pra,pra_a,pra_b
      double precision  pra_c,ton,ton_a,ton_b,ton_c,tonta,nega
      integer           mona,monat,day,atz,periode,ngz,rueck,typ
 
      parameter	(j0    = 1.0098216d11,
     .           j1    = 1.26227808d10,
     .		 j2    = 1.262304d8,
     .		 j3    = 3.1536d7,
     .		 t     = 8.64d4,
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
      print *, 'Programm zur Umrechnung von Daten ATZ/NGZ nach da Ark'
      print *, 'V 4.0 beta (30.09.2002 by Christian Dalhoff)'
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
      endselect
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
        selectcase (atz)
        case (-42,-39,-36,-33,-30,-27,-24,-21,-18,-15,-12,-9)
             goto 09
        case default
          print *, 'Dieses Jahr hatte keinen 29. Februar!'
          goto 01
        endselect
      endif
09    if (atz >= -8) then
          atz=abs(atz)
      endif
      if ((atz <= -9) .and. (atz >= -45)) then
        atz=abs(atz)
        s2=(int((atz-1)/3))-2
        if (mona < 3) then
          selectcase (atz)
          case (42,39,36,33,30,27,24,21,18,15,12,9)
          feb=t
          endselect
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
! Rechnet um von Sekunden seit 1 Atz auf Sekunden seit 1 da Ark
      time = zeit+dif
!Kleine Vorsichtsmassnahme
      vre1  = 0
      vre2  = 0
      vre3  = 0
      daark = 0
      peri  = 0
      prago = 0
      tonta = 0
! Die folgenden if-Schleifen teilen da Ark in die Abschnitte
! -  1    bis  6500 da Ark (vor der Errichtung Tiga Rantons)
! -  6500 bis  8750 da Ark (Veraenderte tonta-Laenge waehrend der
!                           Errichtung Tiga Rantons)
! -  8750 bis 14000 da Ark (Zwischenperiode mit normaler tonta-Laenge)
! - 14000 bis 16250 da Ark (Veraenderte tonta-Laenge waehrend der
!                          letzten Feinabstimmung Tiga Rantons)
! - 16251 bis 19299 da Ark (normale tonta-Laenge)
! - 19300 bis ?     da Ark (nach der Zerstoerung Tiga Rantons)
! - vor 1 da Ark
! ein.
!Kommentare nur im ersten Durchlauf, danach entsprechend!
! 1 da Ark bis 6500 da Ark
      if ((time >= 0) .and. (time <= 2.42421011071d11)) then
! Prueft wieviele Schaltperioden seit 1 da Ark stattfanden.
	vre1    = int(time/(ark1))
! Berechnet die	Anzahl der Jahre da Ark	in dieser Schaltperiode.
	vre2    = int((time-vre1*ark1)/ark2)
! Loesst das Problem, dass ein Jahr mit den Pragos der Vretatou laenger 
! als ein Standardjahr da Ark ist.
        if (vre2==50) then
           vre2=49
        endif
! Berechnet das	exakte Datum und die Uhrzeit da	Ark.
	daark   = vre1*50+vre2+1
	peri    = (time-vre1*ark1-vre2*ark2)/per
	periode	= int(peri)
	prago   = int((time-vre1*ark1-vre2*ark2-periode*per)/pra)
	tonta   = (time-vre1*ark1-vre2*ark2-periode*per-prago*pra)/ton
      endif                                    
!6501-8750 da Ark                              
      if ((time>2.42421011071d11) .and. (time<=3.26335812093d11)) then
         time    = time-2.42421011071d11
         vre1    = int(time/(ark1a))
         vre2    = int((time-vre1*ark1a)/ark2a)
         if (vre2==50) then
            vre2=49
         endif
         daark   = (vre1+130)*50+vre2+1
         peri    = (time-vre1*ark1a-vre2*ark2a)/per_a
         periode = int(peri)
         prago   = int((time-vre1*ark1a-vre2*ark2a-periode*per_a)/pra_a)
         tonta   = (time-vre1*ark1a-vre2*ark2a-periode*per_a
     .            -prago*pra_a)/ton_a
      endif
!8751-14000 da Ark
      if ((time>3.26335812093d11) .and. (time<=5.22137397958d11)) then
        time = time-3.26335812093d11
	vre1    = int(time/(ark1))
	vre2    = int((time-vre1*ark1)/ark2)
        if (vre2==50) then
           vre2=49      
        endif
       	daark   = (vre1+175)*50+vre2+1
	peri    = (time-vre1*ark1-vre2*ark2)/per
	periode	= int(peri)
	prago   = int((time-vre1*ark1-vre2*ark2-periode*per)/pra)
	tonta   = (time-vre1*ark1-vre2*ark2-periode*per-prago*pra)
     .            /ton
      endif
! 14001-16250 da Ark 
      if ((time>5.22137397958d11) .and. (time<=6.06052527678d11)) then
        time = time-5.22137397958d11
	vre1    = int(time/(ark1b))
        vre2    = int((time-vre1*ark1b)/ark2b)
        if (vre2==50) then
           vre2=49
        endif
	daark   = (vre1+280)*50+vre2+1
	peri    = (time-vre1*ark1b-vre2*ark2b)/per_b
	periode	= int(peri)
	prago = int((time-vre1*ark1b-vre2*ark2b-periode*per_b)/pra_b)
	tonta   = (time-vre1*ark1b-vre2*ark2b-periode*per_b-prago
     .  	  *pra_b)/ton_b
      endif
! 16250 da Ark bis 19300 da Ark
      if ((time>6.06052527678d11) .and. (time<=7.19765528807d11)) then
	vre1    = int(time/(ark1))
	vre2    = int((time-vre1*ark1)/ark2)
        if (vre2==50) then
           vre2=49
        endif
	daark   = vre1*50+vre2+1
	peri    = (time-vre1*ark1-vre2*ark2)/per
	periode	= int(peri)
	prago   = int((time-vre1*ark1-vre2*ark2-periode*per)/pra)
	tonta   = (time-vre1*ark1-vre2*ark2-periode*per-prago*pra)
     .            /ton
      endif
! 19300 da Ark  
      if ((time>7.19765528807d11) .and. (time<=7.19804027314d11)) then
	daark =	19300
	peri  = (time-7.19765528807d11)/per_c
	periode	= int(peri)
	prago =	int((time-7.19765528807d11-periode*per_c)/pra_c)
	tonta =	(time-7.19765528807d11-periode*per_c-prago*pra_c)/ton_c
      endif
! 19301 da Ark - 21499 da Ark
      if (time>7.19804027314d11) then
	time  = time-7.19804027313d11
	vre1  = int(time/ark1c)
	vre3  = int((time-vre1*ark1c)/(25*ark2c+pra_c))
	vre2  = int(((time-vre1*ark1c)-vre3*(25*ark2c+pra_c))/ark2c)
        if (vre2 == 25) then
           vre2 = 24
        endif
        if (vre3 == 2) then
           vre3 = 1
           vre2 = 24
        endif
	daark =	vre1*50+vre2+vre3*25+19301
	peri =(time-vre1*ark1c-vre3*(25*ark2c+pra_c)-vre2*ark2c)/per_c
	periode	= int(peri)
      if(((daark/50)-int(daark/50)/=0) .and. (peri>10.1388888889)) then
           peri = peri+0.5
      endif
	prago =	int((time-vre1*ark1c-vre2*ark2c-vre3*(25*ark2c+pra_c)
     .          -periode*per_c)/pra_c)
	tonta =	(time-vre1*ark1c-vre2*ark2c-vre3*(25*ark2c+pra_c)
     .          -periode*per_c-prago*pra_c)/ton_c+0.00005
      endif

! 21500 da Ark  
      if ((time>8.0182479123038d11) .and. (time<=8.01863187602d11)) then
	daark =	21500
	peri  = (time-7.19765528807d11)/per
	periode	= int(peri)
	prago =	int((time-7.19765528807d11-periode*per)/pra)
	tonta =	(time-7.19765528807d11-periode*per-prago*pra)/ton
      endif
! 21501 da Ark - 
      if (time>8.01863187602d11) then
	time  = time-8.01863187602d11
	vre1  = int(time/ark1)
	vre2  = int((time-vre1*ark1)/ark2)
        if (vre2==50) then
           vre2=49
        endif
	daark =	vre1*50+vre2+21501
	peri =(time-vre1*ark1-vre2*ark2)/per
	periode	= int(peri)
	prago =	int((time-vre1*ark1-vre2*ark2-periode*per)/pra)
	tonta =	(time-vre1*ark1-vre2*ark2-periode*per-prago*pra)/ton+0.00005
      endif

! Berechnet Daten kleiner 1 da Ark
	if (time < 0) then
	  vre1    = int(abs(time)/(ark1))
	  vre2    = int((abs(time)-vre1*ark1)/ark2)
	  daark	  = -(vre1*50+vre2+1)
	  nega    = ark2-(abs(time)-(vre1*ark1)-(vre2*ark2))
	  peri    = nega/per
	  periode = int(peri)
	  prago	  = int((nega-periode*per)/pra)
	  tonta	  = ((nega-periode*per-prago*pra)/ton)
	endif
! Wandelt die Datumsangabe in Ausgabeform um
      if (peri < 10) then
	 print *, prago+1, 'Prago'
      endif
      if (peri >= 10) then
	 if (peri <= 10.1388888889) then
	    print *, prago+1,  'Prago der Katanen des Capits'
	 endif
         if ((peri > 10.1388888889) .and. (peri < 10.4444445)) then
	    print *, prago-4,  'Prago des Vretatou'
	 endif
	 if (peri >= 10.4444445) then
	    print *, 'Tiga Ranton'
	 endif
      endif
      select case (periode)
       case (0)
	 print *, 'des Eyilon'
       case (1)
	 print *, 'der Hara'
       case (2)
	 print *, 'des Tarman'
       case (3)
	 print *, 'der Dryhan'
       case (4)
	 print *, 'des Messon'
       case (5)
	 print *, 'des Tedar'
       case (6)
	 print *, 'des Ansoor'
       case (7)
	 print *, 'der Prikur'
       case (8)
	 print *, 'der Coroma'
      end select
      if ((periode == 9) .and. (prago <= 36)) then
	 print *, 'des Tartor'
      endif
      print *, daark, 'da Ark'
      print *, 't', int(tonta),	'.', int((tonta-int(tonta))*1d4+0.00005)
      if ((daark >=1774) .and. (daark <= 1808)) then
        print *, 'Imperator Gwalon I. (1774-1808 da Ark, *1725 da Ark)'
      endif
      if ((daark >=1808) .and. (daark <= 1824)) then
      print *, 'Imperator Volgathir I. (1808-1824 da Ark, *1767 da Ark)'
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
