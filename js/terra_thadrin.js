function terra_thadrin(typ, jahr, mona, tag, std, min, sec) {
    //	 ZEITEINHEITEN IN SEKUNDEN
    //        j0:    3200*365.2421875 Tage in Sekunden
    //	 j1:	400*3562425 Tage in Sekunden
    //	 j2:	4*36525 Tage in Sekunden
    //	 j3:	365 Tage in Sekunden
    //	 mon:	Dauer der Vormonate in Sekunden
    //	 t:	ein Tag in Sekunden
    //	 s:	eine Stunde in Sekunden
    //	 m:	eine Minute in Sekunden
    //        tha:   ein Jahr Thadrin in Sekunden
    //        burd:  ein Burdrin in Sekunden
    //        sef:   eine Sefrin in Sekunden
    //        ad:    ein Adrin in Sekunden
    //        of:    eine Offrin in Sekunden

    //	 EINGABEWERTE
    //	 typ:	Schaltete um zwischen ATZ und NGZ
    //	 ngz:	Eingabe Jahr
    //	 atz:	Eingabe Jahr
    //	 mona:	Eingabe Monat
    //	 tag:	Eingabe Tag
    //	 std:	Eingabe Stunde
    //	 min:	Eingabe Minute
    //	 sec:	Eingabe Sekunde

    //	 ZWISCHENWERTE UND TESTWERTE
    //        s0:    Anzahl der 3200 Jahre-Schaltzyklen seit 1600 ATZ
    //	 s1:    Anzahl der 400 Jahre Schaltzyklen seit 1 ATZ
    //	 s2:    Anzahl der 4 Jahre Schaltzyklen seit 1 ATZ
    //	 s3:    Anzahl der Jahre im aktuellen Schaltzyklus ATZ
    //	 feb:	fuegt in Schaltjahren den 29.02. ein
    //	 zeit:	Anzahl der Sekunden seit 1. Januar 1 ATZ
    //	 rueck: Regelt Neustart oder Abbruch des Programms
    //        time:  Anzahl der Sekunden seit 1/0.001 0:00:00
    //        dif:   Thadrin in Sekunden am 1. Januar 1 ATZ

    //        AUSGABEWERTE
    //        Thadrin:  Jahreszahl Thadrin
    //        Burdrin:  Thadrin-Tag
    //        Sefrin:   Thadrin-Stunde
    //        Adrin:    Thadrin-Minute
    //        Ofrin:    Thadrin-Sekunde

    /*
                              double precision j0, j1, j2, j3, mon, t, m, sec, s, s0, s1, s2, s3, feb, rest
                              double precision zeit, time, dif, thadrin, tha, burdrin, bur, sefrin, sef
                              double precision adrin, ad, ofrin, of
                              integer atz, ngz, mona, monat, tag, day, std, min, rueck, typ
                          */

    const j0 = 1.0098216e11;
    const j1 = 1.26227808e10;
    const j2 = 1.262304e8;
    const j3 = 3.1536e7;
    const t = 8.64e4;
    const s = 3.6e3;
    const m = 6.0e1;
    const dif = 5.3556345072e12;
    const tha = 8.34e7;
    const bur = 8.34e4;
    const sef = 8.34e3;
    const ad = 8.34e1;
    const of = 0.834;

    //Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.
    /*
                        Programm zur Umrechnung von ATZ/NGZ-Daten nach Tahdrin'
                        V 1.0 (06.10.2002 by Christian Dalhoff)'
                        Konvertierung nach JavaScript 2021 by bertholdm
                                                        */

    // 'Waehlen sie das Eingabeformat (ATZ = 0, NGZ = 1)'
    if (typ != Math.floor(typ) || typ >= 2 || typ < 0) {
        return "Kein zulaessiges Datumsformat.";
    }
    // 'Geben sie das Jahr ATZ ein:';
    // 'Geben sie das Jahr NGZ ein:';
    if (jahr == 0) {
        return "Ein Jahr 0 ATZ/NGZ existiert nicht.";
    }
    if (jahr != Math.floor(jahr)) {
        return "Jahreseingaben muessen ganzzahlig sein.";
    }
    if (typ == 0) {
        atz = jahr;
    }
    if (typ == 1) {
        // Umrechnung von NGZ in ATZ, die if Schleifen sind noetig, da beide
        // Systeme kein Jahr 0 kennen
        if (jahr >= 1) {
            atz = jahr + 3587;
        }
        if (jahr < 1) {
            atz = jahr + 3588;
        }
    }
    //  'Geben sie den Monat ein (1-12):'
    if (mona != Math.floor(mona)) {
        return "Monatseingaben muessen ganzzahlig sein.";
    }
    if (mona > 12 || mona < 1) {
        return "Monate muessen im Bereich 1-12 liegen";
    }
    //  'Geben sie den Tag ein (1-31):';
    if (tag != Math.floor(tag)) {
        return "Tageseingaben muessen ganzzahlig sein.";
    }
    if (tag > 31 || tag < 1) {
        return "Tage muessen im Bereich 1-31 liegen";
    }
    if (atz == 4 && mona == 2) {
        if (tag == 29) {
            return "Das Jahr 4 ATZ war kein Schaltjahr.";
        }
    }
    if ((atz - 1600) % 3200 == 0 && mona == 2) {
        if (tag == 29) {
            return "Dieses Jahr ATZ ist kein Schaltjahr.";
        }
    }
    if (tag == 31) {
        switch (mona) {
            case 4:
            case 6:
            case 9:
            case 11:
                return "Dieser Monat hat nur 30 Tage.";
        }
    }
    //  'Geben sie die Stunde ein (0-23):'
    if (std > 23 || std < 0) {
        return "Stunden muessen im Bereich 0-23 liegen";
    }
    if (std != Math.floor(std)) {
        return "Stundeneingaben muessen ganzzahlig sein.";
    }
    //  'Geben sie die Minute ein (0-59):'
    if (min > 59 || min < 0) {
        return "Minuten muessen im Bereich 0-59 liegen.";
    }
    if (min != Math.floor(min)) {
        return "Minuteneingaben muessen ganzzahlig sein.";
    }
    //  'Geben sie die Sekunde ein (0-59):';
    if (sec > 59 || sec < 0) {
        return "Sekunden muessen im Bereich 0-59 liegen.";
    }
    if (sec != Math.floor(sec)) {
        return "Sekundeneingaben muessen ganzzahlig sein.";
    }

    //     Die folgende case-Anweisung ordnet dem Monat die Zahl der seit
    //     Jahresbeginn bis zum letzten des Vormonats vergangenen Sekunden zu.
    switch (mona) {
        case 1:
            mon = 0;
            break;
        case 2:
            mon = 2.6784e6;
            break;
        case 3:
            mon = 5.0976e6;
            break;
        case 4:
            mon = 7.776e6;
            break;
        case 5:
            mon = 1.0368e7;
            break;
        case 6:
            mon = 1.30464e7;
            break;
        case 7:
            mon = 1.56384e7;
            break;
        case 8:
            mon = 1.83168e7;
            break;
        case 9:
            mon = 2.09952e7;
            break;
        case 10:
            mon = 2.35872e7;
            break;
        case 11:
            mon = 2.62656e7;
            break;
        case 12:
            mon = 2.88576e7;
            break;
    }

    // Kleine Vorsichtsmassnahme
    s0 = 0;
    s1 = 0;
    s2 = 0;
    s3 = 0;
    rest = 0;
    feb = 0;

    // Berechnungen fuer positive Jahreszahlen ATZ.
    if (atz > 0) {
        s0 = Math.floor((atz - 1601) / 3200);
        s1 = Math.floor((atz - s0 * 3200 - 1) / 400);
        rest = atz - s0 * 3200 - s1 * 400 - 1;
        if (atz < 5) {
            s3 = atz - 1;
        }
        if (atz >= 5 && atz < 1582) {
            // korrekte julianische Schaltung ab 8 ATZ bis zur gregorianischen
            // Kalenderreform
            s2 = Math.floor((atz - 1) / 4) - 1;
            // beruecksichtigt den ausgefallenen Schalttag 4 ATZ
            s3 = atz - 1 - (s2 * 4 + 4);
            // Fuegt den 29. Februar  ein, wenn das aktuelle Jahr ein Schaltjahr ist.
            if (s3 == 3 && mona > 2) {
                feb = t;
            }
            if (s3 != 3 && mona == 2) {
                if (tag > 28) {
                    return "In diesem Jahr hat der Februar nur 28 Tage.";
                }
            }
            s3 = s3 + 4;
        }
        // Ab 1582 ATZ
        if (atz > 1582) {
            s2 = Math.floor(rest / 4);
            s3 = rest - s2 * 4;
            if (s3 == 3 && mona > 2) {
                feb = t;
            }
            if (s3 != 3 && mona == 2) {
                if (tag > 28) {
                    return "In diesem Jahr hat der Februar nur 28 Tage.";
                }
            }
            if (atz % 100 == 0) {
                feb = 0;
            }
            if (atz % 400 == 0 && mona > 2) {
                feb = t;
            }
            if ((atz - 1600) % 3200 == 0) {
                feb = 0;
            }
        }
        // Berechnet die Anzahl der Sekunden seit 1 ATZ.
        zeit =
            s0 * j0 +
            s1 * j1 +
            s2 * j2 +
            s3 * j3 +
            mon +
            t * (tag - 1) +
            std * s +
            min * m +
            sec +
            feb;
        if (atz > 1582) {
            zeit = zeit + 2 * t;
        }
        // Schleife fuer das Korrekturjahr 1582 ATZ.
        if (atz == 1582) {
            if (mona <= 9) {
                zeit = 4.98924576e10 + mon + t * (tag - 1) + std * s + min * m + sec;
            }
            if (mona == 10 && tag <= 4) {
                zeit = 4.99160448e10 + t * (tag - 1) + std * s + min * m + sec;
            }
            if (mona == 10 && tag > 4) {
                if (tag <= 14) {
                    return "Der 5.-14. Oktober 1582 existieren nicht.";
                }
                if (tag >= 15) {
                    s2 = Math.floor(rest / 4);
                    s3 = rest - s2 * 4;
                    zeit =
                        s1 * j1 +
                        s2 * j2 +
                        s3 * j3 +
                        mon +
                        (t * (tag - 1) + std * s + min * m + sec + 2 * t);
                }
            }
            if (mona > 10) {
                s2 = Math.floor(rest / 4);
                s3 = rest - s2 * 4;
                zeit =
                    s1 * j1 +
                    s2 * j2 +
                    s3 * j3 +
                    mon +
                    t * (tag - 1) +
                    std * s +
                    min * m +
                    sec +
                    2 * t;
            }
        }
    }
    // Fuehrt o.g. Operationen auf Jahreszahlen ATZ kleiner 1 aus.
    if (atz < 1) {
        //Prueft ob der 29.Februar zulaessig ist:
        if (mona == 2 && tag == 29 && atz > -46) {
            switch (atz) {
                case -42:
                case -39:
                case -36:
                case -33:
                case -30:
                case -27:
                case -24:
                case -21:
                case -18:
                case -15:
                case -12:
                case -9:
                    break;
                default:
                    return "Dieses Jahr hatte keinen 29. Februar.";
            }
        }

        if (atz >= -8) {
            atz = Math.abs(atz);
        }
        if (atz <= -9 && atz >= -45) {
            atz = Math.abs(atz);
            s2 = Math.floor((atz - 1) / 3) - 2;
            if (mona < 3) {
                switch (atz) {
                    case 42:
                    case 39:
                    case 36:
                    case 33:
                    case 30:
                    case 27:
                    case 24:
                    case 21:
                    case 18:
                    case 15:
                    case 12:
                    case 9:
                        feb = t;
                }
            }
        }
    }
    if (atz < -45) {
        atz = Math.abs(atz);
        // Prueft ob der 29. Februar zulaessig ist
        if (tag == 29 && mona == 2) {
            if ((atz + 3) % 100 == 0 && (atz + 3) % 400 != 0) {
                return "Dieses Jahr hatte keinen 29. Februar.";
            }
            if ((atz + 3) / 4 != Math.floor((atz + 3) / 4)) {
                return "Dieses Jahr hatte keinen 29. Februar.";
            }
        }
        s0 = Math.floor((atz - 2) / 3200);
        rest = atz - s0 * 3200;
        s1 = Math.floor((rest - 2) / 400);
        rest = atz - s0 * 3200 - s1 * 400;
        s2 = Math.floor((rest - 2) / 4);
        s3 = rest - s2 * 4 - 1;
        if (s3 == 4 && mona < 3) {
            feb = t;
        }
        if (s3 == 4 && mona >= 3) {
            feb = 0;
        }
        if (s3 != 4) {
            feb = 0;
        }
        if ((atz + 3) % 100 == 0) {
            feb = 0;
        }
        if ((atz + 3) % 400 == 0 && mona < 3) {
            feb = t;
        }
        if ((atz + 3) % 3200 == 0) {
            feb = 0;
        }
    }

    // Berechnet die Anzahl der Sekunden bis 1 ATZ.
    zeit = -(
        s0 * j0 +
        s1 * j1 +
        s2 * j2 +
        s3 * j3 +
        feb +
        t +
        (j3 - (mon + (tag - 1) * t + std * s + min * m + sec))
    );

    // Rechnet um in Sekunden seit 1 Thadrin
    time = zeit + dif;
    if (time >= 0) {
        thadrin = Math.floor(time / tha);
        burdrin = Math.floor((time - thadrin * tha) / bur);
        sefrin = Math.floor((time - thadrin * tha - burdrin * bur) / sef);
        adrin = Math.floor(
            (time - thadrin * tha - burdrin * bur - sefrin * sef) / ad
        );
        ofrin = Math.floor(
            (time - thadrin * tha - burdrin * bur - sefrin * sef - adrin * ad) / of
        );
        return (
            time +
            " " +
            (thadrin + 1).toString() +
            " /" +
            ((burdrin + 1) / 1000).toString() +
            sefrin +
            adrin +
            ofrin
        );
    }
    // Berechnet Daten kleiner 1 Thadrin
    if (time < 0) {
        time = Math.abs(time);
        thadrin = -Math.floor(time / tha);
        time = time - thadrin * tha;
        burdrin = Math.floor((tha - time) / bur);
        sefrin = Math.floor((tha - time - burdrin * bur) / sef);
        adrin = Math.floor((tha - time - burdrin * bur - sefrin * sef) / ad);
        ofrin = Math.floor(
            (tha - time - burdrin * bur - sefrin * sef - adrin * ad) / of
        );
        return (
            time +
            " " +
            (thadrin - 1).toString() +
            " /" +
            ((burdrin + 1) / 1000).toString() +
            sefrin +
            adrin +
            ofrin
        );
    }
}