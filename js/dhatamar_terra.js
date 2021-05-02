function dhatamar_terra(dha, torlon, ty, co1, co2, co3) {
    //	 ZEITEINHEITEN IN SEKUNDEN
    //   j0:  3200*365.2421875 Tage in Sekunden (durchschnittliches Jahr)
    //	 j1:	400*356,2425 Tage in Sekunden (gregorianisch)
    //	 j2:	4*365,25 Tage in Sekunden (julianisch)
    //	 j3:	365 Tage in Sekunden
    //	 mon:	Dauer der Vormonate in Sekunden
    //	 t:	ein Tag in Sekunden
    //	 s:	eine Stunde in Sekunden
    //	 m:	eine Minute in Sekunden
    //
    //	 EINGABEWERTE
    //	 typ:	Schaltete um zwischen ATZ und NGZ
    //	 ngz:	Eingabe Jahr
    //	 atz:	Eingabe Jahr
    //	 mona:	Eingabe Monat
    //	 tag:	Eingabe Tag
    //	 std:	Eingabe Stunde
    //	 min:	Eingabe Minute
    //	 sec:	Eingabe Sekunde
    //
    //	 ZWISCHENWERTE UND TESTWERTE
    //   s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
    //	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
    //	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
    //	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
    //	 feb:	fuegt in Schaltjahren den 29.02. ein
    //	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
    //	 rueck: Regelt Neustart oder Abbruch des Programms
    //        time:
    //        dif:
    //        v0:    5450 Periode dha-tamar
    //        v1:    50 Jahre-Periode dha-tamar
    //        AUSGABEWERTE

    /*
                                                                                                          double precision  j0,j1,j2,j3,mon,t,tag,s,std,m,min,sec,s0,s1,s2
                                                                                                          double precision  s3,feb,rest,zeit,time,dif,nega,mem,dha
                                                                                                          double precision	v0,v1,vr0,vr1,vr2,co1,co2,co3,c1,c2,c3,ty
                                                                                                          integer           mona,monat,day,atz,ngz,ber,tor,rueck,typ,tamar
                                                                                                          integer           torlon
                                                                                                      */

    let mon;
    let s0, s1, s2, s3;
    let tor;
    const j0 = 1.0098216e11;
    const j1 = 1.26227808e10;
    const j2 = 1.262304e6;
    const j3 = 3.1536e7;
    const t = 8.64e4;
    const s = 3.6e3;
    const m = 6.0e1;
    const v0 = 1.71985248e11;
    const v1 = 1.5778368e9;
    const c1 = 7.2e3;
    const c2 = 6.0e2;
    const c3 = 5.0e1;
    // "Das Jahr 56.400 v. Chr. (0 dha-Tamar) ist der Beginn der lemurischen Zeitrechnung.""
    // (https://www.perrypedia.de/wiki/Lemurer)
    // 1,77981057e12 = 56400 Jahre * 365,2421875 Tage * 24 Stunden * 60 Minuten * 60 Sekunden
    const dif = 1.779780211e12;
    let time; // time ist in der Fortran-Quelle ohne Zuweisung!
    // Möglicherweise ist in einigen Zuweisungen "zeit" durch "time zu ersetzen.

    /* 
                          Programm zur Umrechnung von dha-Tamar nach ATZ/NGZ'
                          V 1.0 beta (28.12.2002 by Christian Dalhoff)'
                          Konvertierung nach JavaScript 2021 by bertholdm
                          */

    /*
                          Zuordnung der Torlon-Nummern:
                          Jannhis  =  1
                          Keub     =  2
                          Nazhach  =  3
                          Uhs      =  4
                          Fohlad   =  5
                          Sikkhla  =  6
                          Adomet   =  7
                          Aizhidos =  8
                          Illhach  =  9
                          Thiodege = 10
                          Ezrah    = 11
                          Eizhel   = 12
                          Berlen`ty der Vrehetatou = 13
                        */

    // Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.

    if (dha == 0) {
        return "Ein Jahr 0 dha-Tamar existiert nicht.";
    }
    if (dha != Math.floor(dha)) {
        return "Jahreseingaben muessen ganzzahlig sein.";
    }
    if (torlon != Math.floor(torlon)) {
        return "Torlon muessen ganzzahlig sein.";
    }
    if (torlon > 13 || torlon < 1) {
        return "Torlon muessen im Bereich 1-13 liegen";
    }
    if (torlon == 13) {
        if (dha > 0) {
            if (Math.floor(dha / 50) != dha / 50) {
                return "Dieses Jahr dha ist kein Schaltjahr.";
            }
        }
        if (dha < 0) {
            dha = dha + 1;
            if (math.ceil(dha / 50) != dha / 50) {
                return "Im negativen Bereich wird -1, -51, ...geschaltet.";
            }
            dha = dha - 1;
        }
    }

    if (ty != Math.floor(ty)) {
        return "Ty muessen ganzzahlig sein.";
    }
    if (ty > 36 || ty < 1) {
        return "Ty muessen im Bereich 1-36 liegen";
    }
    if (ty > 24) {
        switch (torlon) {
            case 2:
            case 4:
            case 6:
            case 8:
            case 10:
                return "Dieser Torlon hat nur 24 Ty";
        }
    }
    if (torlon == 11 && ty > 33) {
        return "Der Ezrah hat nur 33 Ty";
    }
    if (torlon == 12 && ty > 32) {
        return "Der Eizhel hat nur 32 Ty";
    }
    if (torlon == 13) {
        if (dha > 0) {
            if (Math.floor(dha / 5450) != dha / 5450 && ty > 12) {
                return "Dieser torlon hat nur alle 5450 Jahre 24 Ty";
            }
            if (ty > 12) {
                if (Math.floor(dha / 5450) == dha / 5450 && ty < 25) {
                    return "Dieser Torlon hat nur 12 Ty (Alle 5450 Jahre sind 24 Ty erlaubt)";
                }
            }
        }
    }
    if (co1 > 11 || co1 < 0) {
        return "Coberlen`ty muessen im Bereich 0-11 liegen";
    }
    if (co1 != Math.floor(co1)) {
        return "Coberlen`ty muessen ganzzahlig sein.";
    }
    if (co2 > 11 || co2 < 0) {
        return "Cobol`ty muessen im Bereich 0-11 liegen.";
    }
    if (co2 != Math.floor(co2)) {
        return "Cobol`ty muessen ganzzahlig sein.";
    }
    if (co3 >= 12 || co3 < 0) {
        return "Sekunden muessen im Bereich 0-11.99 liegen.";
    }

    //     Die folgende case-Anweisung ordnet dem Torlon die Zahl der seit
    //     Jahresbeginn bis zum letzten des Vortorlon vergangenen Sekunden zu.
    switch (torlon) {
        case 1:
            tor = 0;
            break;
        case 2:
            tor = 36 * t;
            break;
        case 3:
            tor = 60 * t;
            break;
        case 4:
            tor = 96 * t;
            break;
        case 5:
            tor = 120 * t;
            break;
        case 6:
            tor = 156 * t;
            break;
        case 7:
            tor = 180 * t;
            break;
        case 8:
            tor = 216 * t;
            break;
        case 9:
            tor = 240 * t;
            break;
        case 10:
            tor = 276 * t;
            break;
        case 11:
            tor = 300 * t;
            break;
        case 12:
            tor = 333 * t;
            break;
        case 13:
            tor = 365 * t;
            break;
    }

    // Berechnungen fuer positive Jahreszahlen dha-Tamar.
    if (dha > 0) {
        // Berechnet die Anzahl der kompletten 5450-Jahre Schaltzyklen.
        s0 = Math.floor((dha - 1) / 5450);
        // Berechnet die Anzahl der kompletten 50-Jahre Schaltzyklen.
        s1 = Math.floor((dha - 1 - s0 * 5450) / 50);
        // Berechnet die Anzahl der kompletten Jahre seit dem letzten Schaltjahr
        s2 = dha - s0 * 5450 - s1 * 50 - 1;
        // Berechnet die Anzahl der Sekunden seit 1 dha-Tamar.
        // zeit =
        time =
            s0 * v0 +
            s1 * v1 +
            s2 * j3 +
            tor +
            t * (ty - 1) +
            co1 * c1 +
            co2 * c2 +
            co3 * c3;
    }

    // Fuehrt o.g. Operationen auf Jahreszahlen dha-Tamar kleiner 1 aus.
    if (dha == -1) {
        // Berechnet die Anzahl der Sekunden bis 1 ATZ.
        // zeit = -(
        time = -(j3 + 24 * t - tor - (ty - 1) * t - co1 * c1 - co2 * c2 - co3 * c3);
    }
    if (dha < -1) {
        dha = Math.abs(dha);
        s0 = Math.floor((dha - 2) / 5450);
        rest = dha - s0 * 5450;
        s1 = Math.floor((rest - 2) / 50);
        s2 = dha - 5450 * s0 - s1 * 50 - 1;
        // Berechnet die Anzahl der Sekunden bis 1 ATZ.
        // zeit = -(
        time = -(
            s0 * v0 +
            s1 * v1 +
            s2 * j3 +
            24 * t +
            j3 -
            tor -
            (ty - 1) * t -
            co1 * c1 -
            co2 * c2 -
            co3 * c3
        );
    }

    // Kleine Vorsichtsmassnahme
    co1 = 0;
    co2 = 0;
    co3 = 0;
    dha = 0;
    torlon = 0;
    ty = 0;
    ber = 0;
    s0 = 0;
    s1 = 0;
    s2 = 0;
    s3 = 0;
    rest = 0;

    // Rechnet um in Sekunden seit 1 atz
    // time ist in der Fortran-Quelle undefiniert!
    // Analog zu daark_terra und domm_terra wurde in den obenstehenden Anweisungen
    // "zeit" durch "time" ersetzt
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
    if (zeit >= 0 && zeit < 1.26143999e8) {
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
    // Ab 15.10.1582 ATZ  ;
    if (zeit > 4.99163904e10) {
        zeit = zeit - 2 * t;
        s0 = Math.floor((zeit - 5.04911232e10) / j0);
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
    // Ordnet den Monat zu.;
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
            // Beruecksichtigt, dass -45 ATZ kein Schaltjahr war
            if (atz == 45) {
                s3 = 0;
            }
        }
        // Bis -45 ATZ;
        if (zeit < -1.4201568e9) {
            // -t gleicht die Fehler zu Beginn der julianischen Schaltung aus
            zeit = Math.abs(zeit) - t;
            s0 = Math.floor(zeit / j0);
            s1 = Math.floor((zeit - s0 * j0) / j1);
            s2 = Math.floor((zeit - s0 * j0 - s1 * j1) / j2);
            s3 = Math.floor((zeit - s0 * j0 - s1 * j1 - s2 * j2) / j3);
            atz = -(s0 * 3200 + s1 * 400 + s2 * 4 + s3 + 1);
            mona = zeit - s0 * j0 - s1 * j1 - s2 * j2 - s3 * j3;
            mon = j3 - mona;
            s3 = s3 + 1;
            // Korrektur fuer nicht durch 4 teilbare Jahrhunderte
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