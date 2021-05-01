      program Terra_Thadrin
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
!        tha:   ein Jahr Thadrin in Sekunden
!        burd:  ein Burdrin in Sekunden
!        sef:   eine Sefrin in Sekunden
!        ad:    ein Adrin in Sekunden
!        of:    eine Offrin in Sekunden

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
!        time:  Anzahl der Sekunden seit 1/0.001 0:00:00 
!        dif:   Thadrin in Sekunden am 1. Januar 1 ATZ



!        AUSGABEWERTE
!        Thadrin:  Jahreszahl Thadrin
!        Burdrin:  Thadrin-Tag
!        Sefrin:   Thadrin-Stunde
!        Adrin:    Thadrin-Minute
!        Ofrin:    Thadrin-Sekunde

      double precision  j0,j1,j2,j3,mon,t,m,sec,s,s0,s1,s2,s3,feb,rest 
      double precision  zeit,time,dif,thadrin,tha,burdrin,bur,sefrin,sef
      double precision  adrin,ad,ofrin,of
      integer           atz,ngz,mona,monat,tag,day,std,min,rueck,typ

      parameter	(j0  = 1.0098216d11,
     .           j1  = 1.26227808d10,
     .		 j2  = 1.262304d8,
     .		 j3  = 3.1536d7,
     .		 t   = 8.64d4,
     .		 s   = 3.6d3,
     .		 m   = 6.d1,      
     .           dif = 5.3556345072d12,
     .           tha = 8.34d7,
     .           bur = 8.34d4,
     .           sef = 8.34d3,
     .           ad  = 8.34d1,
     .           of  = 0.834)

* Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.

      print *, 'Programm zur Umrechnung von ATZ/NGZ-Daten nach Tahdrin'
      print *, 'V 1.0 (06.10.2002 by Christian Dalhoff)'
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
      
! Rechnet um in Sekunden seit 1 Thadrin
      time = zeit+dif
      print *, time
      if (time >= 0) then
      thadrin = int(time/tha)
      burdrin = int((time-thadrin*tha)/bur)
      sefrin = int((time-thadrin*tha-burdrin*bur)/sef)
      adrin = int((time-thadrin*tha-burdrin*bur-sefrin*sef)/ad)
      ofrin = int((time-thadrin*tha-burdrin*bur-sefrin*sef-adrin*ad)/of)
       print *, thadrin+1, ' /', (burdrin+1)/1000
       print *, sefrin, adrin, ofrin
      endif
! Berechnet Daten kleiner 1 Thadrin
      if (time < 0) then
        time=abs(time)
        thadrin = -int(time/tha)
        time = time-thadrin*tha
        burdrin = int((tha-time)/bur)
        sefrin  = int((tha-time-burdrin*bur)/sef)
        adrin = int((tha-time-burdrin*bur-sefrin*sef)/ad)
        ofrin = int((tha-time-burdrin*bur-sefrin*sef-adrin*ad)/of)
        print *, thadrin-1, ' /', (burdrin+1)/1000
        print *, sefrin, adrin, ofrin
      endif
      print *, 'Eine weitere Umrechnung	durchfuehren 0 = nein 1 = ja?'
      read (*,*), rueck
      if (rueck	/= 1) then
        goto 10
        endif
      if (rueck	== 1) then
        goto 01
      endif
 10   end



