function Domm_Terra(domm, rhyn, kado, croz, hiddyn) {
    //	 ZEITEINHEITEN IN SEKUNDEN
    //	 j1:	400 tropische Terra-Jahre in Sekunden
    //	 j2:	36525 Tage in Sekunden
    //	 j3:	365 Tage in Sekunden
    //	 mon:	Dauer der Vormonate in Sekunden
    //	 t:	ein Tag in Sekunden
    //	 s:	eine Stunde in Sekunden
    //	 m:	eine Minute in Sekunden
    //        dom:   ein Jahr Domm in Sekunden
    //        rhy:   ein Rhyn in Sekunden
    //        kad:   eine Kado in Sekunden
    //        cro:   ein Croz in Sekunden
    //        hid:   eine Hiddyn in Sekunden

    //   EINGABEWERTE
    //   domm:  Jahreszahl da Ark
    //   rhyn:  dommrathischer Standardmonat
    //   kado:  dommrathische  Standardwoche
    //   croz:  dommrathischer Standardtag
    //   hiddyn:dommrathische Standardsekunde

    //	 ZWISCHENWERTE UND TESTWERTE
    //   s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
    //	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
    //	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
    //	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
    //	 feb:	fuegt in Schaltjahren den 29.02. ein
    //	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
    //	 rueck:	Regelt Neustart oder Abbruch des Programms
    //        time:  Anzahl der Sekunden seit 1. Croz 1 Domm
    //        dif:   Domm in Sekunden am 1. Januar 1 ATZ

    //  AUSGABEWERTE
    //  ngz:      Jahr NGZ
    //  atz:      Jahr ATZ
    //  monat:    Monat
    //  tage:     Tag
    //  stunde:   Stunde
    //  minute:   Minute
    //  sekunde:  Sekunde

    /*
                    double precision  j0, j1, j2, j3, mon, t, m, s, sec, s1, s2, s0, s3, feb, rest
                    double precision  zeit,dif,time,dom,domm,rhy,rhyn,kad,kado,cro
                    double precision  hiddyn,croz,hid
                    integer           mona,monat,day,tag,std,min,atz,ngz,rueck,typ
                    */

    const j0 = 1.0098216e11;
    const j1 = 1.26227808e10;
    const j2 = 1.262304e8;
    const j3 = 3.1536e7;
    const t = 8.64e4;
    const s = 3.6e3;
    const m = 6.0e1;
    const dif = 3.23884083826e13;
    const dom = 7.5e7;
    const rhy = 7.5e6;
    const kad = 7.5e5;
    const cro = 7.5e4;
    const hid = 0.75;

    /* Programm zur Umrechnung von Daten Domm nach ATZ/NGZ
            V 2.0 (05.10.2002 by Christian Dalhoff)
            Konvertierung nach JavaScript 2021 by bertholdm
            */

    // Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.

    // 'Geben sie das Jahr Domm ein:';
    if (domm == 0) {
        return "Ein Jahr 0 Domm existiert nicht.";
    }
    if (domm != Math.floor(domm)) {
        return "domm-Eingaben muessen ganzzahlig sein.";
    }
    // 'Geben sie den Rhyn (1-10) ein:';
    if (rhyn > 10 || rhyn < 1) {
        return "Rhyn muessen im Bereich 1-10 liegen";
    }
    if (rhyn != Math.floor(rhyn)) {
        return "Rhyn muessen ganzzahlig sein.";
    }
    // 'Geben sie die Kado ein (1-10):';
    if (kado > 10 || kado < 1) {
        return "Kados muessen im Bereich 1-10 liegen";
    }
    if (kado != Math.floor(kado)) {
        return "Kados muessen ganzzahlig sein.";
    }
    // 'Geben sie den Croz ein (1-10):';
    if (croz > 10 || croz < 0) {
        return "Croz muessen im Bereich 1-10 liegen.";
    }
    if (croz != Math.floor(croz)) {
        return "Croz muessen ganzzahlig sein.";
    }
    // 'Geben sie die Hiddyn ein (0-99999):';
    if (hiddyn > 99999 || hiddyn < 0) {
        return "Hiddyn muessen im Bereich 0-99999 liegen.";
    }
    if (hiddyn != Math.floor(hiddyn)) {
        return "Hiddyn muessen ganzzahlig sein.";
    }

    // Berechnet fuer positive Jahreszahlen Domm die seit 1 Domm
    // vergangenen Sekunden
    if (croz >= 1) {
        time =
            (domm - 1) * dom +
            (rhyn - 1) * rhy +
            (kado - 1) * kad +
            (croz - 1) * cro +
            hiddyn * hid;
    }
    // Fuehrt o.g. Operationen auf Jahreszahlen Domm kleiner 1 aus.
    if (domm < 1) {
        domm = Math.abs(domm);
        time = -(
            (domm - 1) * dom +
            dom -
            ((rhyn - 1) * rhy + (kado - 1) * kad + (croz - 1) * cro + hiddyn * hid)
        );
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
        day = mon - 2.36736e7;
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
    // -1 ATZ bis -9 ATZ
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
        // Berechnung des Monats
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
    // ToDo: Ersetzung der Konkatenation durch Template Literal
    return (
        (tag + 1).toString() +
        " " +
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
        Math.floor(sec) +
        "sec"
    );
}