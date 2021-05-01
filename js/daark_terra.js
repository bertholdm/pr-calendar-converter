function daark_terra(daark, periode, prago, tonta) {
    //	 ZEITEINHEITEN IN SEKUNDEN
    //   j0:    3200*365.2421875 Tage in Sekunden
    //	 j1:	400*3562425 Tage in Sekunden
    //	 j2:	4*36525 Tage in Sekunden
    //	 j3:	365 Tage in Sekunden
    //	 mon:	Dauer der Vormonate in Sekunden
    //	 t:	ein Tag in Sekunden
    //	 s:	eine Stunde in Sekunden
    //	 m:	eine Minute in Sekunden
    //        ark1:  ein Jahr da Ark in Sekunden (Variiert daher a-c)
    //        per:   eine Periode in Sekunden (Variiert daher a-c)
    //        pra:   ein Prago in Sekunden (Variiert daher a-c)
    //        ton:   eine Tonta in Sekunden (Variiert daher a-c)
    //	 EINGABEWERTE

    //	 ZWISCHENWERTE UND TESTWERTE
    //   s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
    //	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
    //	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
    //	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
    //	 feb:	fuegt in Schaltjahren den 29.02. ein
    //	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
    //	 rueck: Regelt Neustart oder Abbruch des Programms
    //        time:  Anzahl der Sekunden seit 1. Prago des Eyilon 1 da Ark
    //        dif:   da Ark in Sekunden am 1. Januar 1 ATZ
    //        vre1:  Anzahl der Schaltperioden seit 1 da Ark
    //        vre2:  Anzahl der Jahre im aktuellen Schaltzyklus da Ark
    //        vre3:  Anzahl der 25 Jahre Schaltperioden innerhalb dieser
    //               50 Jahre Periode
    //        neg:   Zwischenwert bei da Ark Werten kleiner 0

    //   AUSGABEWERTE
    //	 ngz:	   Jahreszahl NGZ
    //	 atz:	   Eingabe Jahr
    //	 mona:	   Eingabe Monat
    //	 tage:	   Eingabe Tag
    //	 stun:	   Eingabe Stunde
    //	 minu:	   Eingabe Minute
    //	 sec:	   Eingabe Sekunde

    /*
                        double precision j0, j1, j2, j3, mon, t, m, sec, s, s0, s1, s2, s3, feb, rest
                        double precision zeit,time,dif
                        double precision ark1,ark1a,ark1b,ark2,ark1c,ark2a,ark2b,ark2c
                        double precision per,per_a,per_b,per_c,peri,prago,pra,pra_a
                        double precision pra_b,pra_c,ton,ton_a,ton_b,ton_c,tonta
                        double precision daark,vre1,vre2,vre3,nega
                        integer          atz,ngz,mona,monat,tag,day,std,min,rueck,typ
                        integer          periode
                    */

    const j0 = 1.0098216e11;
    const j1 = 1.26227808e10;
    const j2 = 1.262304e8;
    const j3 = 3.1536e7;
    const t = 8.64e4;
    const s = 3.6e3;
    const m = 6.0e1;
    const ark1 = 1.86477700824e9;
    const ark1a = 1.86477335604e9;
    const ark1b = 1.86478066044e9;
    const ark1c = 1.86498323748e9;
    const ark2 = 3.72730742022e7;
    const ark2a = 3.72730012022e7;
    const ark2b = 3.72730815022e7;
    const ark2c = 3.72731140383e7;
    const per = 3.67624841447e6;
    const per_a = 3.67624121447e6;
    const per_b = 3.67625561447e6;
    const per_c = 3.67625234351e6;
    const pra = 1.02118011513e5;
    const pra_a = 1.02117811513e5;
    const pra_b = 1.02118211513e5;
    const pra_c = 1.02118120653e5;
    const ton = 5.10590057565e3;
    const ton_a = 5.10589057565e3;
    const ton_b = 5.10591057565e3;
    const ton_c = 5.10590603263e3;
    const dif = 6.44647557545e11;

    /*
Programm zur Umrechnung von Daten da Ark nach ATZ/NGZ"
V 4.0 beta (30.09.2002 by Christian Dalhoff)
Konvertierung nach JavaScript by Michael Detambel
*/
    let vre1 = 0;
    let vre2 = 0;
    let vre3 = 0;
    let peri = 0;

    /*
    Zuordnung der Perioden-Nummern:
    Eyilon =  1
    Hara   =  2
    Tarman =  3
    Dryhan =  4
    Messon =  5
    Tedar  =  6
    Ansoor =  7
    Prikur =  8
    Coroma =  9
    Tartor = 10
    Katanen des Capit   = 11
    Pragos des Vretatou = 12
    Tiga Ranton = 13
*/
    // Nimmt die Eingabewerte an und ueberprueft; ob sie zulaessig sind.

    // ("Geben sie das Jahr da Ark ein:")
    if (daark == 0) {
        return "Ein Jahr 0 da Ark existiert nicht!";
    }
    if (daark != Math.floor(daark)) {
        return "da Ark-Eingaben muessen ganzzahlig sein!";
    }
    // ("Geben sie die Periode (1-13) ein:")
    if (periode > 13 || periode < 1) {
        return "Perioden muessen im Bereich 1-13 liegen!";
    }
    if (periode != Math.floor(periode)) {
        return "Perioden muessen ganzzahlig sein!";
    }
    if (periode == 12 && daark / 50 - Math.floor(daark / 50) != 0) {
        return "In diesem Jahr gab es keine Pragos des Vretatou!";
    }
    if (periode == 12) {
        periode = 11;
        peri = 5;
    }
    if (periode == 13 && daark < 19300) {
        return "Tiga Ranton wurde erst 19300 da Ark eingefuehrt!";
    }
    if (periode == 13 && daark > 21499) {
        return "Tiga Ranton wurde 21500 da Ark abgeschafft!";
    }
    if (periode == 13 && daark / 25 - Math.floor(daark / 25) != 0) {
        return "In diesem Jahr gab es kein Tiga Ranton!";
    }
    if (periode == 13) {
        periode = 11;
        peri = 16;
    }
    // ("Geben sie den Prago ein (1-36):")
    if (prago > 36 || prago < 1) {
        return "Pragos muessen im Bereich 1-36 liegen!";
    }
    if (prago != Math.floor(prago)) {
        return "Pragos muessen ganzzahlig sein!";
    }
    if (periode == 11) {
        if (prago > 5 && peri == 0) {
            return "Die Katanen des Capits haben maximal 5 Pragos!";
        }
        if (peri == 5 && prago > 11) {
            return "Die Pragos des Vretatou haben maximal 11 Pragos!";
        }
        if (peri == 16 && prago > 1) {
            return "Tiga Ranton ist nur ein Prago!";
        }
    }
    // ("Geben sie die Tonta ein (0-19.9999):")
    if (tonta > 20 || tonta < 0) {
        return "Tontas muessen im Bereich 0-19.9999 liegen!";
    }

    // Berechnet fuer positive Jahreszahlen da Ark die seit 1 da Ark
    // vergangenen kompletten Schaltperioden und die Anzahl der Jahre seit der
    // Letzten.
    if (daark > 0) {
        vre1 = Math.floor((daark - 1) / 50);
        vre2 = daark - 1 - vre1 * 50;
        // Bis 6500 da Ark;
        if (daark <= 6500) {
            // Berechnet die Anzahl der Sekunden seit 1 da Ark.
            time =
                vre1 * ark1 +
                vre2 * ark2 +
                (periode - 1) * per +
                peri * pra +
                (prago - 1) * pra +
                tonta * ton;
        }
        // 6501 da Ark bis 8750 da Ark
        if (daark > 6500 && daark <= 8750) {
            vre1 = vre1 - 130;
            time =
                2.42421011071e11 +
                vre1 * ark1a +
                vre2 * ark2a +
                (periode - 1) * per_a +
                peri * pra_a +
                (prago - 1) * pra_a +
                tonta * ton_a;
        }
        // 8751 da Ark bis 14000 da Ark
        if (daark > 8750 && daark <= 14000) {
            vre1 = vre1 - 175;
            time =
                3.26335812093e11 +
                vre1 * ark1 +
                vre2 * ark2 +
                (periode - 1) * per +
                peri * pra +
                (prago - 1) * pra +
                tonta * ton;
        }
        // 14001 da Ark bis 16250 da Ark
        if (daark > 14000 && daark <= 16250) {
            vre1 = vre1 - 280;
            time =
                5.22137397958e11 +
                vre1 * ark1b +
                vre2 * ark2b +
                (periode - 1) * per_b +
                peri * pra_b +
                (prago - 1) * pra_b +
                tonta * ton_b;
        }
        // 16250 bis zur Zerstoerung von ArkonIII
        if (daark >= 16251 && daark < 19300) {
            time =
                vre1 * ark1 +
                vre2 * ark2 +
                (periode - 1) * per +
                peri * pra +
                (prago - 1) * pra +
                tonta * ton;
        }
        // Nach der Zerstoerung von ArkonIII
        if (daark == 19300) {
            time =
                7.19765528807e11 +
                (periode - 1) * per_c +
                peri * pra_c +
                (prago - 1) * pra_c +
                tonta * ton_c;
        }
        if (daark >= 19301 && daark < 21500) {
            vre3 = Math.floor((daark - 19301) / 25);
            vre2 = daark - 19301 - vre3 * 25;
            if (vre3 / 2 - Math.floor(vre3 / 2) == 0 && peri == 16) {
                peri = 5;
            }
            time =
                7.19804027313e11 +
                vre2 * ark2c +
                vre3 * (25 * ark2c + pra_c) +
                Math.floor(vre3 / 2) * 11 * pra_c +
                (periode - 1) * per_c +
                (prago - 1) * pra_c +
                tonta * ton_c +
                peri * pra_c;
        }
        // Nach der Wiederherstellung von Tiga Ranton
        if (daark == 21500) {
            time =
                8.0182479123038e11 +
                (periode - 1) * per +
                peri * pra +
                (prago - 1) * pra +
                tonta * ton;
        }
        if (daark >= 21501) {
            vre1 = (daark - 21501) / 50;
            vre2 = daark - 21501 - vre1 * 50;
            time =
                8.01863187602e11 +
                vre1 * ark1 +
                vre2 * ark2 +
                (periode - 1) * per +
                (prago - 1) * pra +
                tonta * ton +
                peri * pra;
        }
    }
    // Rechnet um in Sekunden seit 1 atz
    zeit = time - dif;
    // Kleine Vorsichtsmassnahme
    s0 = 0;
    s1 = 0;
    s2 = 0;
    s3 = 0;
    mon = 0;
    mona = 0;
    tag = 0;
    // 1 ATZ bis 4 ATZ;
    if (zeit >= 0 && zeit < 1.26143999) {
        s3 = Math.floor(zeit / j3);
        atz = s3 + 1;
        mon = zeit - s3 * j3;
    }
    // 5 ATZ bis 4.10.1582 ATZ;
    if (zeit >= 1.26143999e8 && zeit <= 4.99163904e10) {
        s2 = Math.floor(zeit / j2) - 1;
        s3 = Math.floor((zeit - s2 * j2) / j3);
        atz = s2 * 4 + s3 + 1;
        mon = zeit - (s2 * j2 + s3 * j3);
        s3 = s3 - 4;
    }
    // Ab 15.10.1582 ATZ;
    if (zeit > 4.99163904e10) {
        zeit = zeit - 2 * t;
        s0 = Math.floor((zeit - 5.04911232) / j0);
        s1 = Math.floor((zeit - s0 * j0) / j1);
        s2 = Math.floor((zeit - s0 * j0 - s1 * j1) / j2);
        s3 = Math.floor((zeit - s0 * j0 - s1 * j1 - s2 * j2) / j3);
        atz = s0 * 3200 + s1 * 400 + s2 * 4 + s3 + 1;
        mon = zeit - s0 * j0 - s1 * j1 - s2 * j2 - s3 * j3;
        // Korrektur fuer nicht durch 4 teilbare Jahrhunderte
        if (atz % 100 == 0) {
            s3 = 0;
        }
        if (atz % 400 == 0) {
            s3 = 3;
        }
        if ((atz - 1600) % 3200 == 0) {
            s3 = 0;
        }
    }
    // Ordnet den Monat zu.
    if (mon <= 2.6784e6) {
        monat = 1;
        day = Math.floor(mon);
    }
    if (mon > 2.6784e6) {
        monat = 2;
        day = Math.floor(mon - 2.6784e6);
    }
    if (mon > 5.0976e6 && s3 != 3) {
        monat = 3;
        day = Math.floor(mon - 5.0976e6);
    }
    if (mon > 5.184e6 && s3 == 3) {
        monat = 3;
        day = Math.floor(mon - 5.184e6);
    }
    if (mon > 7.776e6 && s3 != 3) {
        monat = 4;
        day = Math.floor(mon - 7.776e6);
    }
    if (mon > 7.8624e6 && s3 == 3) {
        monat = 4;
        day = Math.floor(mon - 7.8624e6);
    }
    if (mon > 1.0368e7 && s3 != 3) {
        monat = 5;
        day = Math.floor(mon - 1.0368e7);
    }
    if (mon > 1.04544e7 && s3 == 3) {
        monat = 5;
        day = Math.floor(mon - 1.04544e7);
    }
    if (mon > 1.30464e7 && s3 != 3) {
        monat = 6;
        day = Math.floor(mon - 1.30464e7);
    }
    if (mon > 1.31328e7 && s3 == 3) {
        monat = 6;
        day = Math.floor(mon - 1.31328e7);
    }
    if (mon > 1.56384e7 && s3 != 3) {
        monat = 7;
        day = Math.floor(mon - 1.56384e7);
    }
    if (mon > 1.57248e7 && s3 == 3) {
        monat = 7;
        day = Math.floor(mon - 1.57248e7);
    }
    if (mon > 1.83168e7 && s3 != 3) {
        monat = 8;
        day = Math.floor(mon - 1.83168e7);
    }
    if (mon > 1.84032e7 && s3 == 3) {
        monat = 8;
        day = Math.floor(mon - 1.84032e7);
    }
    if (mon > 2.09952e7 && s3 != 3) {
        monat = 9;
        day = Math.floor(mon - 2.09952e7);
    }
    if (mon > 2.10816e7 && s3 == 3) {
        monat = 9;
        day = Math.floor(mon - 2.10816e7);
    }
    if (mon > 2.35872e7 && s3 != 3) {
        monat = 10;
        day = Math.floor(mon - 2.35872e7);
    }
    if (mon > 2.36736e7 && s3 == 3) {
        monat = 10;
        day = Math.floor(mon - 2.36736e7);
    }
    if (mon > 2.62656e7 && s3 != 3) {
        monat = 11;
        day = Math.floor(mon - 2.62656e7);
    }
    if (mon > 2.6352e7 && s3 == 3) {
        monat = 11;
        day = Math.floor(mon - 2.6352e7);
    }
    if (mon > 2.88576e7 && s3 != 3) {
        monat = 12;
        day = Math.floor(mon - 2.88576e7);
    }
    if (mon > 2.8944e7 && s3 == 3) {
        monat = 12;
        day = Math.floor(mon - 2.8944e7);
    }
    // -1 ATZ bis -9 ATZ;
    if (zeit < 0) {
        if (zeit < 0 && zeit > -2.52288e8) {
            zeit = Math.abs(zeit);
            s3 = Math.floor(zeit / j3);
            atz = -(s3 + 1);
            mona = Math.floor(zeit - s3 * j3);
            mon = j3 - mona;
            s3 = 0;
        }
        // -9 ATZ bis -45 ATZ;
        if (zeit < -2.52288e8 && zeit >= -1.4201568e9) {
            zeit = Math.abs(zeit);
            s2 = Math.floor((zeit - 6 * j3) / (j3 * 3 + t));
            s3 = Math.floor((zeit - s2 * (j3 * 3 + t) - 6 * j3) / j3);
            atz = -(s2 * 3 + s3 + 7);
            mona = zeit - s2 * (3 * j3 + t) - (s3 + 6) * j3;
            mon = j3 - mona;
            s3 = s3 - 1;
            // Beruecksichtigt; dass -45 ATZ kein Schaltjahr war;
            if (atz == 45) {
                s3 = 0;
            }
        }
        // Bis -45 ATZ;
        if (zeit < -1.4201568e9) {
            // -t gleicht die Fehler zu Beginn der julianischen Schaltung aus//;
            zeit = Math.abs(zeit) - t;
            s0 = Math.floor(zeit / j0);
            s1 = Math.floor((zeit - s0 * j0) / j1);
            s2 = Math.floor((zeit - s0 * j0 - s1 * j1) / j2);
            s3 = Math.floor((zeit - s0 * j0 - s1 * j1 - s2 * j2) / j3);
            atz = -(s0 * 3200 + s1 * 400 + s2 * 4 + s3 + 1);
            mona = zeit - s0 * j0 - s1 * j1 - s2 * j2 - s3 * j3;
            mon = j3 - mona;
            s3 = s3 + 1;
            // Korrektur fuer nicht durch 4 teilbare Jahrhunderte;
            if ((atz + 3) % 100 == 0) {
                s3 = 0;
            }
            if ((atz + 3) % 400 == 0) {
                s3 = 1;
            }
            if ((atz + 3) % 3200 == 0) {
                s3 = 0;
            }
        }
        // Berechnung des Monats;
        if (mon <= 2.6784e6) {
            monat = 1;
            day = Math.floor(mon);
        }
        if (mon > 2.6784e6) {
            monat = 2;
            day = Math.floor(mon - 2.6784e6);
        }
        if (mon > 5.0976e6 && s3 != 1) {
            monat = 3;
            day = Math.floor(mon - 5.0976e6);
        }
        if (mon > 5.184e6 && s3 == 1) {
            monat = 3;
            day = Math.floor(mon - 5.184e6);
        }
        if (mon > 7.776e6 && s3 != 1) {
            monat = 4;
            day = Math.floor(mon - 7.776e6);
        }
        if (mon > 7.8624e6 && s3 == 1) {
            monat = 4;
            day = Math.floor(mon - 7.8624e6);
        }
        if (mon > 1.0368e7 && s3 != 1) {
            monat = 5;
            day = Math.floor(mon - 1.0368e7);
        }
        if (mon > 1.04544e7 && s3 == 1) {
            monat = 5;
            day = Math.floor(mon - 1.04544e7);
        }
        if (mon > 1.30464e7 && s3 != 1) {
            monat = 6;
            day = Math.floor(mon - 1.30464e7);
        }
        if (mon > 1.31328e7 && s3 == 1) {
            monat = 6;
            day = Math.floor(mon - 1.31328e7);
        }
        if (mon > 1.56384e7 && s3 != 1) {
            monat = 7;
            day = Math.floor(mon - 1.56384e7);
        }
        if (mon > 1.57248e7 && s3 == 1) {
            monat = 7;
            day = Math.floor(mon - 1.57248e7);
        }
        if (mon > 1.83168e7 && s3 != 1) {
            monat = 8;
            day = Math.floor(mon - 1.83168e7);
        }
        if (mon > 1.84032e7 && s3 == 1) {
            monat = 8;
            day = Math.floor(mon - 1.84032e7);
        }
        if (mon > 2.09952e7 && s3 != 1) {
            monat = 9;
            day = Math.floor(mon - 2.09952e7);
        }
        if (mon > 2.10816e7 && s3 == 1) {
            monat = 9;
            day = Math.floor(mon - 2.10816e7);
        }
        if (mon > 2.35872e7 && s3 != 1) {
            monat = 10;
            day = Math.floor(mon - 2.35872e7);
        }
        if (mon > 2.36736e7 && s3 == 1) {
            monat = 10;
            day = Math.floor(mon - 2.36736e7);
        }
        if (mon > 2.62656e7 && s3 != 1) {
            monat = 11;
            day = Math.floor(mon - 2.62656e7);
        }
        if (mon > 2.6352e7 && s3 == 1) {
            monat = 11;
            day = Math.floor(mon - 2.6352e7);
        }
        if (mon > 2.88576e7 && s3 != 1) {
            monat = 12;
            day = Math.floor(mon - 2.88576e7);
        }
        if (mon > 2.8944e7 && s3 == 1) {
            monat = 12;
            day = Math.floor(mon - 2.8944e7);
        }
    }
    tag = Math.floor(day / t);
    std = Math.floor((day - tag * t) / s);
    min = Math.floor((day - tag * t - std * s) / m);
    sec = Math.floor(day - tag * t - std * s - min * m);
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
        " / " +
        ngz +
        " NGZ " +
        std +
        "h " +
        min +
        "min " +
        Math.floor(sec) +
        "sec"
    );
}