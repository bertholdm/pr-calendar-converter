      program Domm_Terra
      implicit none

!	 ZEITEINHEITEN IN SEKUNDEN
!	 j1:	400 tropische Terra-Jahre in Sekunden
!	 j2:	365,25 Tage in Sekunden
!	 j3:	365 Tage in Sekunden
!	 mon:	Dauer der Vormonate in Sekunden
!	 t:	ein Tag in Sekunden
!	 s:	eine Stunde in Sekunden
!	 m:	eine Minute in Sekunden
!        dom:   ein Jahr Domm in Sekunden
!        rhy:   ein Rhyn in Sekunden
!        kad:   eine Kado in Sekunden
!        cro:   ein Croz in Sekunden
!        hid:   eine Hiddyn in Sekunden

!        EINGABEWERTE
!        domm:  Jahreszahl da Ark
!        rhyn:  dommrathischer Standardmonat
!        kado:  dommrathische  Standardwoche
!        croz:  dommrathischer Standardtag
!        hiddyn:dommrathische Standardsekunde

!	 ZWISCHENWERTE UND TESTWERTE
!        s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
!	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
!	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
!	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
!	 feb:	fuegt in Schaltjahren den 29.02. ein
!	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
!	 rueck:	Regelt Neustart oder Abbruch des Programms
!        time:  Anzahl der Sekunden seit 1. Croz 1 Domm
!        dif:   Domm in Sekunden am 1. Januar 1 ATZ

*        AUSGABEWERTE
*        ngz:      Jahr NGZ
*        atz:      Jahr ATZ
*        monat:    Monat
*        tage:     Tag
*        stunde:   Stunde
*        minute:   Minute
*        sekunde:  Sekunde

      double precision  j0,j1,j2,j3,mon,t,m,s,sec,s1,s2,s0,s3,feb,rest
      double precision  zeit,dif,time,dom,domm,rhy,rhyn,kad,kado,cro
      double precision  hiddyn,croz,hid
      integer           mona,monat,day,tag,std,min,atz,ngz,rueck,typ

      parameter (j0 = 1.0098216d11,
     .           j1 = 1.26227808d10,
     .		 j2 = 1.262304d8,
     .           j3 = 3.1536d7,
     .           t   = 8.64d4,
     .           s   = 3.6d3,
     .           m   = 6.d1,
     .           dif = 3.23884083826d13,
     .           dom = 7.5d7,
     .           rhy = 7.5d6,
     .           kad = 7.5d5,
     .           cro = 7.5d4,
     .           hid = 0.75)

! Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.

      print *, 'Programm zur Umrechnung von Daten Domm nach ATZ/NGZ'
      print *, 'V 2.0 (05.10.2002 by Christian Dalhoff)'
      print *, 'Bitte alle Eingaben mit "Return" bestaetigen!'

 50   print *, 'Geben sie das Jahr Domm ein:'
      read (*, *), domm
      if (domm == 0) then
         print *, 'Ein Jahr 0 Domm existiert nicht!'
         goto 50
      endif
      if (domm /= int(domm)) then
         print *, 'domm-Eingaben muessen ganzzahlig sein!'
         goto 50
      endif
 51   print *, 'Geben sie den Rhyn (1-10) ein:'
      read (*, *), rhyn
      if ((rhyn > 10) .or. (rhyn < 1)) then
         print *, 'Rhyn muessen im Bereich 1-10 liegen'
         goto 51
      endif
      if (rhyn /= int(rhyn)) then
         print *, 'Rhyn muessen ganzzahlig sein!'
         goto 51
      endif
      print *, 'Geben sie die Kado ein (1-10):'
 52   read (*, *), kado
      if ((kado > 10) .or. (kado < 1)) then
         print *, 'Kados muessen im Bereich 1-10 liegen'
         goto 52
      endif
      if (kado /= int(kado)) then
         print *, 'Kados muessen ganzzahlig sein!'
         goto 52
      endif
 53   print *, 'Geben sie den Croz ein (1-10):'
      read (*, *), croz
      if ((croz  > 10) .or. (croz < 0)) then
         print *, 'Croz muessen im Bereich 1-10 liegen!'
         goto 53
      endif
      if (croz /= int(croz)) then
         print *, 'Croz muessen ganzzahlig sein!'
         goto 53
      endif
 54   print *, 'Geben sie die Hiddyn ein (0-99999):'
      read (*, *), hiddyn
      if ((hiddyn  > 99999) .or. (hiddyn < 0)) then
         print *, 'Hiddyn muessen im Bereich 0-99999 liegen!'
         goto 54
      endif
      if (hiddyn /= int(hiddyn)) then
         print *, 'Hiddyn muessen ganzzahlig sein!'
         goto 54
      endif
! Berechnet fuer positive Jahreszahlen Domm die seit 1 Domm
! vergangenen Sekunden
        if (croz >= 1) then
        time = (domm-1)*dom+(rhyn-1)*rhy+(kado-1)*kad+(croz-1)*cro
     .         +hiddyn*hid
        endif
! Fuehrt o.g. Operationen auf Jahreszahlen Domm kleiner 1 aus.
      if (domm < 1) then
        domm = abs(domm)
        time = -((domm-1)*dom+dom-((rhyn-1)*rhy+(kado-1)*kad+(croz-1)
     .         *cro+hiddyn*hid))
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
      print *, tag+1, monat, '.', atz, ' ATZ', ' /', ngz, ' NGZ'
      print *, std, 'h', min, 'min', int(sec), 'sec'
      print *, 'Eine weitere Umrechnung durchfuehren 0 = nein 1 = ja?'
      read (*,*), rueck
        if (rueck /= 1) then
          goto 10
        endif
        if (rueck == 1) then
          goto 50
        endif
10    end



