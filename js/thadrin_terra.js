function Thadrin_Terra(thadrin, burdrin, sefrin, adrin, ofrin) {
    //	 ZEITEINHEITEN IN SEKUNDEN;
    //	 j1:	400 tropische Terra-Jahre in Sekunden;
    //	 j2:	365,25 Tage in Sekunden;
    //	 j3:	365 Tage in Sekunden;
    //	 mon:	Dauer der Vormonate in Sekunden;
    //	 t:	ein Tag in Sekunden;
    //	 s:	eine Stunde in Sekunden;
    //	 m:	eine Minute in Sekunden;
    //        tha:   ein Jahr Thadrin in Sekunden;
    //        burd:  ein Burdrin in Sekunden;
    //        sef:   eine Sefrin in Sekunden;
    //        ad:    ein Adrin in Sekunden;
    //        of:    eine Offrin in Sekunden;

    //        EINGABEWERTE;
    //        Thadrin:  Jahreszahl Thadrin;
    //        Burdrin:  Thadrin-Tag;
    //        Sefrin:   Thadrin-Stunde;
    //        Adrin:    Thadrin-Minute;
    //        Ofrin:    Thadrin-Sekunde;

    //	 ZWISCHENWERTE UND TESTWERTE;
    //        s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ;
    //	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ;
    //	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ;
    //	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ;
    //	 feb:	fuegt in Schaltjahren den 29.02. ein;
    //	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ;
    //	 rueck:	Regelt Neustart oder Abbruch des Programms;
    //        time:  Anzahl der Sekunden seit 1 Thadrin;
    //        dif:   Thadrin in Sekunden am 1. Januar 1 ATZ;

    //        AUSGABEWERTE;
    //       ngz:      Jahr NGZ;
    //       atz:      Jahr ATZ;
    //       monat:    Monat;
    //       tag:      Tag;
    //       stunde:   Stunde;
    //       minute:   Minute;
    //       sekunde:  Sekunde;

    /*
                      double precision  j0,j1,j2,j3,mon,t,m,sec,s,s0,s1,s2,s3,feb,rest ;
                      double precision  zeit,time,dif,thadrin,tha,burdrin,bur,sefrin,sef;
                      double precision  adrin,ad,ofrin,of;
                      integer           atz,ngz,mona,monat,tag,day,std,min,rueck,typ;
                  */

    const j0 = 1.0098216;
    const j1 = 1.26227808;
    const j2 = 1.262304;
    const j3 = 3.1536;
    const t = 8.64;
    const s = 3.6;
    const m = 6.0;
    const dif = 5.3556345072;
    const tha = 8.34;
    const bur = 8.34;
    const sef = 8.34;
    const ad = 8.34;
    const of = 0.834;

    // Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.;

    /*
                       return 'Programm zur Umrechnung von Thadrin-Daten nach ATZ/NGZ';
                       return 'V 1.0 (06.10.2002 by Christian Dalhoff)';
                      */
    //  return 'Geben sie das Jahr Thadrin ein:';
    if (thadrin == 0) {
        return "Ein Jahr 0 Thadrin existiert nicht.";
    }
    if (thadrin != math.floor(thadrin)) {
        return "Thadrin-Eingaben muessen ganzzahlig sein.";
    }

    //   return 'Geben sie den Burdrin (0.001-1.000) ein:';
    burdrin = burdrin * 1000;
    if (burdrin > 1000 || burdrin < 1) {
        return "Burdrin muessen im Bereich 0.001-1.000 liegen";
    }
    if (burdrin != math.floor(burdrin)) {
        return "burdrin muessen ganzzahlig sein.";
    }

    //   return 'Geben sie die Sefrin ein (0-9):';
    if (sefrin > 9 || sefrin < 0) {
        return "Sefrin muessen im Bereich 1-9 liegen";
    }
    if (sefrin != math.floor(sefrin)) {
        return "Sefrin muessen ganzzahlig sein.";
    }

    // return 'Geben sie die Adrin ein (0-99):';
    if (adrin > 99 || adrin < 0) {
        return "Adrin muessen im Bereich 0-99 liegen.";
    }
    if (adrin != math.floor(adrin)) {
        return "Adrin muessen ganzzahlig sein.";
    }

    //return 'Geben sie die Ofrin ein (0-99):';
    if (ofrin > 99 || ofrin < 0) {
        return "Ofrin muessen im Bereich 0-99 liegen.";
    }
    if (ofrin != math.floor(ofrin)) {
        return "Ofrin muessen ganzzahlig sein.";
    }

    // Berechnet fuer positive Jahreszahlen Thadrin die seit 1 Thadrin;
    // vergangenen Sekunden;
    if (thadrin >= 1) {
        time =
            (thadrin - 1) * tha +
            (burdrin - 1) * bur +
            sefrin * sef +
            adrin * ad +
            ofrin * of;
    }
    // Fuehrt o.g. Operationen auf Jahreszahlen Thadrin kleiner 1 aus.;
    if (thadrin < 1) {
        thadrin = abs(thadrin);
        time = -(
            (thadrin - 1) * tha +
            (tha - ((burdrin - 1) * bur + sefrin * sef + adrin * ad + ofrin * of))
        );
    }

    // Rechnet um in Sekunden seit 1 atz
    zeit = time - dif;

    // Kleine Vorsichtsmassnahme;
    s0 = 0;
    s1 = 0;
    s2 = 0;
    s3 = 0;
    mon = 0;
    mona = 0;
    tag = 0;

    // 1 ATZ bis 4 ATZ;
    if (zeit >= 0 && zeit < 1.26143999) {
        s3 = math.floor(zeit / j3);
        atz = s3 + 1;
        mon = zeit - s3 * j3;
    }

    // 5 ATZ bis 4.10.1582 ATZ;
    if (zeit >= 1.26143999 && zeit <= 4.99163904) {
        s2 = math.floor(zeit / j2) - 1;
        s3 = math.floor((zeit - s2 * j2) / j3);
        atz = s2 * 4 + s3 + 1;
        mon = zeit - (s2 * j2 + s3 * j3);
        s3 = s3 - 4;
    }
    // Ab 15.10.1582 ATZ  ;
    if (zeit > 4.99163904) {
        zeit_kopie = zeit;
        zeit = zeit - 2 * t;
        s0 = math.floor((zeit - 5.04911232) / j0);
        s1 = math.floor((zeit - s0 * j0) / j1);
        s2 = math.floor((zeit - s0 * j0 - s1 * j1) / j2);
        s3 = math.floor((zeit - s0 * j0 - s1 * j1 - s2 * j2) / j3);
        atz = s0 * 3200 + s1 * 400 + s2 * 4 + s3 + 1;
        mon = zeit - s0 * j0 - s1 * j1 - s2 * j2 - s3 * j3;
        // Korrektur fuer nicht durch 4 teilbare Jahrhunderte;
        if (mod(atz, 100) == 0) {
            s3 = 0;
        }
        if (mod(atz, 400) == 0) {
            s3 = 3;
        }
        if (mod(atz - 1600, 3200) == 0) {
            s3 = 0;
        }
    }
    // Ordnet den Monat zu.;
    if (mon <= 2.6784) {
        monat = 1;
        day = mon;
    }
    if (mon > 2.6784) {
        monat = 2;
        day = mon - 2.6784;
    }
    if (mon > 5.0976 && s3 != 3) {
        monat = 3;
        day = mon - 5.0976;
    }
    if (mon > 5.184 && s3 == 3) {
        monat = 3;
        day = mon - 5.184;
    }
    if (mon > 7.776 && s3 != 3) {
        monat = 4;
        day = mon - 7.776;
    }
    if (mon > 7.8624 && s3 == 3) {
        monat = 4;
        day = mon - 7.8624;
    }
    if (mon > 1.0368 && s3 != 3) {
        monat = 5;
        day = mon - 1.0368;
    }
    if (mon > 1.04544 && s3 == 3) {
        monat = 5;
        day = mon - 1.04544;
    }
    if (mon > 1.30464 && s3 != 3) {
        monat = 6;
        day = mon - 1.30464;
    }
    if (mon > 1.31328 && s3 == 3) {
        monat = 6;
        day = mon - 1.31328;
    }
    if (mon > 1.56384 && s3 != 3) {
        monat = 7;
        day = mon - 1.56384;
    }
    if (mon > 1.57248 && s3 == 3) {
        monat = 7;
        day = mon - 1.57248;
    }
    if (mon > 1.83168 && s3 != 3) {
        monat = 8;
        day = mon - 1.83168;
    }
    if (mon > 1.84032 && s3 == 3) {
        monat = 8;
        day = mon - 1.84032;
    }
    if (mon > 2.09952 && s3 != 3) {
        monat = 9;
        day = mon - 2.09952;
    }
    if (mon > 2.10816 && s3 == 3) {
        monat = 9;
        day = mon - 2.10816;
    }
    if (mon > 2.35872 && s3 != 3) {
        monat = 10;
        day = mon - 2.35872;
    }
    if (mon > 2.36736 && s3 == 3) {
        monat = 10;
        day = mon - 2.36736;
    }
    if (mon > 2.62656 && s3 != 3) {
        monat = 11;
        day = mon - 2.62656;
    }
    if (mon > 2.6352 && s3 == 3) {
        monat = 11;
        day = mon - 2.6352;
    }
    if (mon > 2.88576 && s3 != 3) {
        monat = 12;
        day = mon - 2.88576;
    }
    if (mon > 2.8944 && s3 == 3) {
        monat = 12;
        day = mon - 2.8944;
    }
    // -1 ATZ bis -9 ATZ;
    if (zeit < 0) {
        if (zeit < 0 && zeit > -2.52288) {
            zeit = abs(zeit);
            s3 = math.floor(zeit / j3);
            atz = -(s3 + 1);
            mona = math.floor(zeit - s3 * j3);
            mon = j3 - mona;
            s3 = 0;
        }
        // -9 ATZ bis -45 ATZ;
        if (zeit < -2.52288 && zeit >= -1.4201568) {
            zeit = abs(zeit);
            s2 = math.floor((zeit - 6 * j3) / (j3 * 3 + t));
            s3 = math.floor((zeit - s2 * (j3 * 3 + t) - 6 * j3) / j3);
            atz = -(s2 * 3 + s3 + 7);
            mona = zeit - s2 * (3 * j3 + t) - (s3 + 6) * j3;
            mon = j3 - mona;
            s3 = s3 - 1;
            // Beruecksichtigt, dass -45 ATZ kein Schaltjahr war;
            if (atz == 45) {
                s3 = 0;
            }
        }
        // Bis -45 ATZ;
        if (zeit < -1.4201568) {
            // -t gleicht die Fehler zu Beginn der julianischen Schaltung aus//;
            zeit = abs(zeit) - t;
            s0 = math.floor(zeit / j0);
            s1 = math.floor((zeit - s0 * j0) / j1);
            s2 = math.floor((zeit - s0 * j0 - s1 * j1) / j2);
            s3 = math.floor((zeit - s0 * j0 - s1 * j1 - s2 * j2) / j3);
            atz = -(s0 * 3200 + s1 * 400 + s2 * 4 + s3 + 1);
            mona = zeit - s0 * j0 - s1 * j1 - s2 * j2 - s3 * j3;
            mon = j3 - mona;
            s3 = s3 + 1;
            // Korrektur fuer nicht durch 4 teilbare Jahrhunderte;
            if (mod(atz + 3, 100) == 0) {
                s3 = 0;
            }
            if (mod(atz + 3, 400) == 0) {
                s3 = 1;
            }
            if (mod(atz + 3, 3200) == 0) {
                s3 = 0;
            }
        }
        // Berechnung des Monats;
        if (mon <= 2.6784) {
            monat = 1;
            day = mon;
        }
        if (mon > 2.6784) {
            monat = 2;
            day = mon - 2.6784;
        }
        if (mon > 5.0976 && s3 != 1) {
            monat = 3;
            day = mon - 5.0976;
        }
        if (mon > 5.184 && s3 == 1) {
            monat = 3;
            day = mon - 5.184;
        }
        if (mon > 7.776 && s3 != 1) {
            monat = 4;
            day = mon - 7.776;
        }
        if (mon > 7.8624 && s3 == 1) {
            monat = 4;
            day = mon - 7.8624;
        }
        if (mon > 1.0368 && s3 != 1) {
            monat = 5;
            day = mon - 1.0368;
        }
        if (mon > 1.04544 && s3 == 1) {
            monat = 5;
            day = mon - 1.04544;
        }
        if (mon > 1.30464 && s3 != 1) {
            monat = 6;
            day = mon - 1.30464;
        }
        if (mon > 1.31328 && s3 == 1) {
            monat = 6;
            day = mon - 1.31328;
        }
        if (mon > 1.56384 && s3 != 1) {
            monat = 7;
            day = mon - 1.56384;
        }
        if (mon > 1.57248 && s3 == 1) {
            monat = 7;
            day = mon - 1.57248;
        }
        if (mon > 1.83168 && s3 != 1) {
            monat = 8;
            day = mon - 1.83168;
        }
        if (mon > 1.84032 && s3 == 1) {
            monat = 8;
            day = mon - 1.84032;
        }
        if (mon > 2.09952 && s3 != 1) {
            monat = 9;
            day = mon - 2.09952;
        }
        if (mon > 2.10816 && s3 == 1) {
            monat = 9;
            day = mon - 2.10816;
        }
        if (mon > 2.35872 && s3 != 1) {
            monat = 10;
            day = mon - 2.35872;
        }
        if (mon > 2.36736 && s3 == 1) {
            monat = 10;
            day = mon - 2.36736;
        }
        if (mon > 2.62656 && s3 != 1) {
            monat = 11;
            day = mon - 2.62656;
        }
        if (mon > 2.6352 && s3 == 1) {
            monat = 11;
            day = mon - 2.6352;
        }
        if (mon > 2.88576 && s3 != 1) {
            monat = 12;
            day = mon - 2.88576;
        }
        if (mon > 2.8944 && s3 == 1) {
            monat = 12;
            day = mon - 2.8944;
        }
    }
    tag = math.floor(day / t);
    std = math.floor((day - tag * t) / s);
    min = math.floor((day - tag * t - std * s) / m);
    sec = math.floor(day - tag * t - std * s - min * m);
    if (tag == 31) {
        switch (monat) {
            case 1:
                monat = 2;
                tag = 0;
                break;
            case 3:
                monat = 4;
                tag = 0;
                break;
            case 5:
                monat = 6;
                tag = 0;
                break;
            case 7:
                monat = 8;
                tag = 0;
                break;
            case 8:
                monat = 9;
                tag = 0;
                break;
            case 10:
                monat = 11;
                tag = 0;
                break;
            case 12:
                monat = 1;
                tag = 0;
                if (atz > 0) {
                    atz = atz - 1;
                }
                if (atz < 0) {
                    atz = atz + 1;
                }
                break;
        }
    }
    if (tag == 30) {
        switch (monat) {
            case 2:
                monat = 3;
                tag = 0;
                break;
            case 4:
                monat = 5;
                tag = 0;
                break;
            case 6:
                monat = 7;
                tag = 0;
                break;
            case 9:
                monat = 10;
                tag = 0;
                break;
            case 11:
                monat = 12;
                tag = 0;
                break;
        }
    }
    if (tag == 29 && monat == 2) {
        monat = 3;
        tag = 0;
    }
    if (atz <= 3587) {
        ngz = atz - 3588;
    }
    if (atz > 3587) {
        ngz = atz - 3587;
    }
    return (
        (tag + 1).toString() +
        "." +
        monat +
        "." +
        atz +
        " ATZ" +
        " /" +
        ngz +
        " NGZ" +
        std +
        "h" +
        min +
        "min" +
        math.floor(sec) +
        "sec"
    );
}