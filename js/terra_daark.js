function terra_daark(typ, jahr, mona, tag, std, min, sec) {
    //	 ZEITEINHEITEN IN SEKUNDEN
    //   j0:    3200*365.2421875 Tage in Sekunden
    //	 j1:	400*3562425 Tage in Sekunden
    //	 j2:	4*36525 Tage in Sekunden
    //	 j3:	365 Tage in Sekunden
    //	 mon:	Dauer der Vormonate in Sekunden
    //	 t:	ein Tag in Sekunden
    //	 s:	eine Stunde in Sekunden
    //	 m:	eine Minute in Sekunden

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
    //   daark:    Jahreszahl da Ark
    //   periode:  Periode da Ark
    //   prago:    Prago da Ark
    //   tonta:    Tontas da Ark

    /*
                            double precision  j0,j1,j2,j3,mon,t,tag,s,std,m,min,sec,s0,s1,s2
                            double precision  s3,feb,rest,zeit,time,dif,vre1,vre2,vre3,daark
                            double precision	ark1,ark1a,ark1b,ark1c,ark2,ark2a,ark2b,ark2c
                            double precision	per,per_a,per_b,per_c,peri,prago,pra,pra_a,pra_b
                            double precision  pra_c,ton,ton_a,ton_b,ton_c,tonta,nega
                            integer           mona,monat,day,atz,periode,ngz,rueck,typ
                        */

    let mon, s0, s1, s2, s3;

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

    // Nimmt die Eingabewerte an und ueberprueft, ob sie zulaessig sind.
    /*
                                'Programm zur Umrechnung von Daten ATZ/NGZ nach da Ark'
                                'V 4.0 beta (30.09.2002 by Christian Dalhoff)'
                                Konvertierung nach JavaScript 2021 by bertholdm
                                */

    // 'Waehlen sie das Eingabeformat (ATZ = 0, NGZ = 1)'
    if (typ != Math.floor(typ) || typ >= 2 || typ < 0) {
        return "Kein zulaessiges Datumsformat.";
    }
    // 'Geben sie das Jahr ATZ ein:';
    // 'Geben sie das Jahr NGZ ein:';
    if (jahr == 0) {
        return "Ein Jahr 0 ATZ/NTZ existiert nicht.";
    }
    if (jahr != Math.floor(jahr)) {
        return "Jahreseingaben muessen ganzzahlig sein.";
    }
    if (typ == 0) {
        atz = jahr;
    }
    if (typ == 1) {
        // Umrechnung von NGZ in ATZ, die if Schleifen sind noetig, da beide;
        // Systeme kein Jahr 0 kennen
        if (jahr >= 1) {
            atz = jahr + 3587;
        }
        if (jahr < 1) {
            atz = jahr + 3588;
        }
    }
    // 'Geben sie den Monat ein (1-12):';
    if (mona != Math.floor(mona)) {
        return "Monatseingaben muessen ganzzahlig sein.";
    }
    if (mona > 12 || mona < 1) {
        return "Monate muessen im Bereich 1-12 liegen";
    }
    // 'Geben sie den Tag ein (1-31):';
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
    // 'Geben sie die Stunde ein (0-23):';
    if (std > 23 || std < 0) {
        return "Stunden muessen im Bereich 0-23 liegen";
    }
    if (std != Math.floor(std)) {
        return "Stundeneingaben muessen ganzzahlig sein.";
    }
    // 'Geben sie die Minute ein (0-59):';
    if (min > 59 || min < 0) {
        return "Minuten muessen im Bereich 0-59 liegen.";
    }
    if (min != Math.floor(min)) {
        return "Minuteneingaben muessen ganzzahlig sein.";
    }
    // 'Geben sie die Sekunde ein (0-59):';
    if (sec > 59 || sec < 0) {
        return "Sekunden muessen im Bereich 0-59 liegen.";
    }
    if (sec != Math.floor(sec)) {
        return "Sekundeneingaben muessen ganzzahlig sein.";
    }
    // Die folgende case-Anweisung ordnet dem Monat die Zahl der seit
    // Jahresbeginn bis zum letzten des Vormonats vergangenen Sekunden zu.
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
            // korrekte julianische Schaltung ab 8 ATZ bis zur gregorianischen;
            // Kalenderreform;
            s2 = Math.floor((atz - 1) / 4) - 1;
            // beruecksichtigt den ausgefallenen Schalttag 4 ATZ;
            s3 = atz - 1 - (s2 * 4 + 4);
            // Fuegt den 29. Februar  ein, wenn das aktuelle Jahr ein Schaltjahr ist.;
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
    }

    // Rechnet um von Sekunden seit 1 Atz auf Sekunden seit 1 da Ark
    time = zeit + dif;
    //Kleine Vorsichtsmassnahme
    vre1 = 0;
    vre2 = 0;
    vre3 = 0;
    daark = 0;
    peri = 0;
    prago = 0;
    tonta = 0;

    // Die folgenden if-Schleifen teilen da Ark in die Abschnitte
    // -  1    bis  6500 da Ark (vor der Errichtung Tiga Rantons)
    // -  6500 bis  8750 da Ark (Veraenderte tonta-Laenge waehrend der
    //                           Errichtung Tiga Rantons)
    // -  8750 bis 14000 da Ark (Zwischenperiode mit normaler tonta-Laenge)
    // - 14000 bis 16250 da Ark (Veraenderte tonta-Laenge waehrend der
    //                          letzten Feinabstimmung Tiga Rantons)
    // - 16251 bis 19299 da Ark (normale tonta-Laenge)
    // - 19300 bis ?     da Ark (nach der Zerstoerung Tiga Rantons)
    // - vor 1 da Ark
    // ein.
    //Kommentare nur im ersten Durchlauf, danach entsprechend
    // 1 da Ark bis 6500 da Ark
    if (time >= 0 && time <= 2.42421011071e11) {
        // Prueft wieviele Schaltperioden seit 1 da Ark stattfanden.
        vre1 = Math.floor(time / ark1);
        // Berechnet die	Anzahl der Jahre da Ark	in dieser Schaltperiode.
        vre2 = Math.floor((time - vre1 * ark1) / ark2);
        // Loesst das Problem, dass ein Jahr mit den Pragos der Vretatou laenger
        // als ein Standardjahr da Ark ist.
        if (vre2 == 50) {
            vre2 = 49;
        }
        // Berechnet das	exakte Datum und die Uhrzeit da	Ark.
        daark = vre1 * 50 + vre2 + 1;
        peri = (time - vre1 * ark1 - vre2 * ark2) / per;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1 - vre2 * ark2 - periode * per) / pra
        );
        tonta =
            (time - vre1 * ark1 - vre2 * ark2 - periode * per - prago * pra) / ton;
    }
    //6501-8750 da Ark
    if (time > 2.42421011071e11 && time <= 3.26335812093e11) {
        time = time - 2.42421011071e11;
        vre1 = Math.floor(time / ark1a);
        vre2 = Math.floor((time - vre1 * ark1a) / ark2a);
        if (vre2 == 50) {
            vre2 = 49;
        }
        daark = (vre1 + 130) * 50 + vre2 + 1;
        peri = (time - vre1 * ark1a - vre2 * ark2a) / per_a;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1a - vre2 * ark2a - periode * per_a) / pra_a
        );
        tonta =
            (time - vre1 * ark1a - vre2 * ark2a - periode * per_a - prago * pra_a) /
            ton_a;
    }
    //8751-14000 da Ark
    if (time > 3.26335812093e11 && time <= 5.22137397958e11) {
        time = time - 3.26335812093e11;
        vre1 = Math.floor(time / ark1);
        vre2 = Math.floor((time - vre1 * ark1) / ark2);
        if (vre2 == 50) {
            vre2 = 49;
        }
        daark = (vre1 + 175) * 50 + vre2 + 1;
        peri = (time - vre1 * ark1 - vre2 * ark2) / per;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1 - vre2 * ark2 - periode * per) / pra
        );
        tonta =
            (time - vre1 * ark1 - vre2 * ark2 - periode * per - prago * pra) / ton;
    }
    // 14001-16250 da Ark ;
    if (time > 5.22137397958e11 && time <= 6.06052527678e11) {
        time = time - 5.22137397958e11;
        vre1 = Math.floor(time / ark1b);
        vre2 = Math.floor((time - vre1 * ark1b) / ark2b);
        if (vre2 == 50) {
            vre2 = 49;
        }
        daark = (vre1 + 280) * 50 + vre2 + 1;
        peri = (time - vre1 * ark1b - vre2 * ark2b) / per_b;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1b - vre2 * ark2b - periode * per_b) / pra_b
        );
        tonta =
            (time - vre1 * ark1b - vre2 * ark2b - periode * per_b - prago * pra_b) /
            ton_b;
    }
    // 16250 da Ark bis 19300 da Ark
    if (time > 6.06052527678e11 && time <= 7.19765528807e11) {
        vre1 = Math.floor(time / ark1);
        vre2 = Math.floor((time - vre1 * ark1) / ark2);
        if (vre2 == 50) {
            vre2 = 49;
        }
        daark = vre1 * 50 + vre2 + 1;
        peri = (time - vre1 * ark1 - vre2 * ark2) / per;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1 - vre2 * ark2 - periode * per) / pra
        );
        tonta =
            (time - vre1 * ark1 - vre2 * ark2 - periode * per - prago * pra) / ton;
    }
    // 19300 da Ark  ;
    if (time > 7.19765528807e11 && time <= 7.19804027314e11) {
        daark = 19300;
        peri = (time - 7.19765528807e11) / per_c;
        periode = Math.floor(peri);
        prago = Math.floor((time - 7.19765528807e11 - periode * per_c) / pra_c);
        tonta = (time - 7.19765528807e11 - periode * per_c - prago * pra_c) / ton_c;
    }
    // 19301 da Ark - 21499 da Ark
    if (time > 7.19804027314e11) {
        time = time - 7.19804027313e11;
        vre1 = Math.floor(time / ark1c);
        vre3 = Math.floor((time - vre1 * ark1c) / (25 * ark2c + pra_c));
        vre2 = Math.floor(
            (time - vre1 * ark1c - vre3 * (25 * ark2c + pra_c)) / ark2c
        );
        if (vre2 == 25) {
            vre2 = 24;
        }
        if (vre3 == 2) {
            vre3 = 1;
            vre2 = 24;
        }
        daark = vre1 * 50 + vre2 + vre3 * 25 + 19301;
        peri =
            (time - vre1 * ark1c - vre3 * (25 * ark2c + pra_c) - vre2 * ark2c) /
            per_c;
        periode = Math.floor(peri);
        if (daark / 50 - Math.floor(daark / 50) != 0 && peri > 10.1388888889) {
            peri = peri + 0.5;
        }

        prago = Math.floor(
            (time -
                vre1 * ark1c -
                vre2 * ark2c -
                vre3 * (25 * ark2c + pra_c) -
                periode * per_c) /
            pra_c
        );
        tonta =
            (time -
                vre1 * ark1c -
                vre2 * ark2c -
                vre3 * (25 * ark2c + pra_c) -
                periode * per_c -
                prago * pra_c) /
            ton_c +
            0.00005;
    }
    // 21500 da Ark
    if (time > 8.0182479123038e11 && time <= 8.01863187602e11) {
        daark = 21500;
        peri = (time - 7.19765528807e11) / per;
        periode = Math.floor(peri);
        prago = Math.floor((time - 7.19765528807e11 - periode * per) / pra);
        tonta = (time - 7.19765528807e11 - periode * per - prago * pra) / ton;
    }
    // 21501 da Ark -
    if (time > 8.01863187602e11) {
        time = time - 8.01863187602e11;
        vre1 = Math.floor(time / ark1);
        vre2 = Math.floor((time - vre1 * ark1) / ark2);
        if (vre2 == 50) {
            vre2 = 49;
        }
        daark = vre1 * 50 + vre2 + 21501;
        peri = (time - vre1 * ark1 - vre2 * ark2) / per;
        periode = Math.floor(peri);
        prago = Math.floor(
            (time - vre1 * ark1 - vre2 * ark2 - periode * per) / pra
        );
        tonta =
            (time - vre1 * ark1 - vre2 * ark2 - periode * per - prago * pra) / ton +
            0.00005;
    }
    // Berechnet Daten kleiner 1 da Ark
    if (time < 0) {
        vre1 = Math.floor(Math.abs(time) / ark1);
        vre2 = Math.floor((Math.abs(time) - vre1 * ark1) / ark2);
        daark = -(vre1 * 50 + vre2 + 1);
        nega = ark2 - (Math.abs(time) - vre1 * ark1 - vre2 * ark2);
        peri = nega / per;
        periode = Math.floor(peri);
        prago = Math.floor((nega - periode * per) / pra);
        tonta = (nega - periode * per - prago * pra) / ton;
    }

    // Wandelt die Datumsangabe in Ausgabeform um
    let pragoname = "";
    let periodename = "";
    let tartorname = "";
    let daarkname = "";
    let tname = "";
    let imperatorname = "";
    if (peri < 10) {
        // prago + 1, 'Prago'
        pragoname = (prago + 1).toString() + " Prago";
    }
    if (peri >= 10) {
        if (peri <= 10.1388888889) {
            pragoname = (prago + 1).toString() + " Prago der Katanen des Capits";
        }
        if (peri > 10.1388888889 && peri < 10.4444445) {
            pragoname = (prago - 4).toString() + " Prago des Vretatou";
        }
        if (peri >= 10.4444445) {
            pragoname = "Tiga Ranton";
        }
    }
    switch (periode) {
        case 0:
            periodename = "des Eyilon";
            break;
        case 1:
            periodename = "der Hara";
            break;
        case 2:
            periodename = "des Tarman";
            break;
        case 3:
            periodename = "der Dryhan";
            break;
        case 4:
            periodename = "des Messon";
            break;
        case 5:
            periodename = "des Tedar";
            break;
        case 6:
            periodename = "des Ansoor";
            break;
        case 7:
            periodename = "der Prikur";
            break;
        case 8:
            periodename = "der Coroma";
            break;
    }
    if (periode == 9 && prago <= 36) {
        periodename = "des Tartor";
    }
    daarkname = daark.toString().trim() + " da Ark";
    tname =
        "t" +
        Math.floor(tonta) +
        "." +
        Math.floor((tonta - Math.floor(tonta)) * 1e4 + 0.00005);
    if (daark >= 1774 && daark <= 1808) {
        imperatorname = "Imperator Gwalon I. (1774-1808 da Ark, *1725 da Ark)";
    }
    if (daark >= 1808 && daark <= 1824) {
        imperatorname = "Imperator Volgathir I. (1808-1824 da Ark, *1767 da Ark)";
    }
    let converted_date =
        pragoname +
        " " +
        periodename +
        " " +
        tartorname +
        " " +
        daarkname +
        " " +
        tname +
        " " +
        imperatorname;
    return converted_date;
}