C=======================================================================
C                                                                      *
C                         DONALD ECCLESTONE PRESENTS                   *
C                                      -                               *
C                        T H E   H O L Y   T E R R O R                 *
C                                                                      *
C                       TTTTT RRRR  EEEEE K   K 77777                  *
C                         T   R   R E     K  K      7                  *
C                         T   RRRR  EEEE  KKK      7                   *
C                         T   R  R  E     K  K    7                    *
C                         T   R   R EEEEE K   K  7                     *
C                                                                      *
C=======================================================================
C                                                                      *
C  COPYRIGHT (C) 1979  DONALD M. ECCLESTONE                            *
C            74 HUNT VILLAGE CRESCENT, LONDON ONTARIO CANADA  N6H 4A4  *
C                                                                      *
C=======================================================================
C THE FOLLOWING PUNCHES MAY BE FOUND IN THIS DECK -                    *
C THE ALPHABET -- ABCDEFGHIJKLMNOPQRSTUVWXYZ                           *
C THE DIGITS -- 0123456789                                             *
C        12-8-7    EXCLAMATION MARK              !!!!!!!!!!!!!!!!!!!!!!!
C        0-8-5     QUOTATION MARK (DOUBLE QUOTE) """""""""""""""""""""""
C        0-8-6     NUMBER SIGN (POUND SIGN)      #######################
C        11-8-3    DOLLAR SIGN                   $$$$$$$$$$$$$$$$$$$$$$$
C        0-8-7     PERCENT SIGN                  %%%%%%%%%%%%%%%%%%%%%%%
C        11-8-7    AMPERSAND                     &&&&&&&&&&&&&&&&&&&&&&&
C        8-4       APOSTROPHE (SINGLE QUOTE)     '''''''''''''''''''''''
C        0-8-4     LEFT PARNETHESIS              (((((((((((((((((((((((
C        12-8-4    RIGHT PARNETHESIS             )))))))))))))))))))))))
C        11-8-4    ASTERISK                      ***********************
C        12        PLUS SIGN                     +++++++++++++++++++++++
C        0-8-3     COMMA                         ,,,,,,,,,,,,,,,,,,,,,,,
C        11        MINUS SIGN (HYPHEN) (DASH)    -----------------------
C        12-8-3    PERIOD (DECIMAL POINT)        .......................
C        0-1       SLASH                         ///////////////////////
C        11-0      COLON                         :::::::::::::::::::::::

C        0-8-2     SEMICOLON                     ;;;;;;;;;;;;;;;;;;;;;;;
C        12-8-6    LEFT ANGLE BRACKET (LESS THAN)<<<<<<<<<<<<<<<<<<<<<<<
C        8-3       EQUAL SIGN                    =======================
C        11-8-6    RIGHT ANGLE BRACKET(GREATER THAN)  >>>>>>>>>>>>>>>>>>
C        12-0      QUESTION MARK                 ???????????????????????
C        8-5       AT SIGN                       @@@@@@@@@@@@@@@@@@@@@@@
C        11-8-5    LEFT SQUARE BRACKET           [[[[[[[[[[[[[[[[[[[[[[[
C        8-7       BACKSLASH                     ^^^^^^^^^^^^^^^^^^^^^^^
C        12-8-5    RIGHT SQUARE BRACKET          ]]]]]]]]]]]]]]]]]]]]]]]
C        8-5       UP-ARROW (CIRCUMFLEX)         !!!!!!!!!!!!!!!!!!!!!!!
C        8-2       BACK-ARROW OR UNDERSCORE      _______________________
C=======================================================================
C                                                                      *
C  A - MAINLINE AND OFT-USED ROUTINES                                  *
C  B - USER COMMAND ROUTINES                                           *
C  C - M-O SHIP OPERATIONS                                             *
C  D - SHIP INITIALIZATION                                             *
C  E - C-O SHIP OPERATIONS                                             *
C  F - SHIP DAMAGE ROUTINES                                            *
C  G - MISC. ROUTINES (C-O WEAPONRY, ION STORMS, M-O NEW TURN, REPAIR) *
C                                                                      *
C=======================================================================
C                                                                      *
C                           TREK7 MODULE A                             *
C                                                                      *
C        MAINLINE AND OFT-USED ROUTINES                                *
C                                                                      *
C        MAINLINE  GAMOVR    RANDO     HORTA     CYRANO    DI          *
C        ANG       FOSTER    IXIF      LOKI      ILLDAT    OOPS        *
C        ILLDAS                                                        *
C                                                                      *
C=======================================================================
      INTEGER CMAND(26)
      LOGICAL ITAKA(6)
      LOGICAL GAMOVR,SULU,SAREK,SPOCK,HARPO,LARRY,CURLY
      COMMON /A/IT,IS,II(2),IJ(2),I3,JS,ISHAK,NOSTOP
      COMMON /B/NDEAB(2),IPHOB(2),IONB(2),IGOB(2),IFIB(2)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /E/PHASR(4),TWARP(4),IPHOT(4),NDEAD(4),ISURR(4)
      COMMON /F/WARP(4),ITROW(4),ITCOL(4)
      COMMON /G/ZAP(4),ICOLA(4),IROWA(4),LOCK(4),ICOIL(4)
      COMMON /H/ANGLE(4),RANG(4),LOCKT(4)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /J/KODE(2,8),STATIC(4)
      COMMON /K/IWHO(8),DIST(8),KILLZ,KILLR,KILLD,KILLG,IGOCO(8)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /O/MA(4,33),K(4,14),NOMAP(4),MANUM(4),HIVEL(4,2),ITEMP(4),
     *NOMOV(4)
      COMMON /P/IPULL(4),IPUSH(4),PULL(4),PUSH(4),IPULLR(4),IPULLC(4),
     *IPUSHR(4),IPUSHC(4)
      COMMON /Q/IARMZ(6),INDUZ,IXRYZ,IMAGZ,IMAGRZ,IABSZ,ISTAZ,INVIZ(4),
     *ICLOZ,IONNO,IPLANZ,IRUNZ,IDEVZ,IDRIZ,IMAXZ
      COMMON /R/IBPSC(4),IBPSB(8),IBPSS(2),IBPSE(25)
      COMMON /S/IBPOB(8),IBPOE(25)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      COMMON /U/LAUNCH,NUMOUT,NUME(2)
      COMMON /V/IWEB(2),IWEBZ,INVIS(4)
      COMMON /W/IDEX(2,33,20)
        COMMON /TOM/ITOM
      EQUIVALENCE(II(1),I1),(II(2),I2)
      DATA CMAND/'IN','MA','TR','CO','PH','TO','GO','CA','EA',
     *'BO','YA','DE','HE','MI','RE','RA','CE','AT','DA','CR','SP',
     *'EN','NO','SU','HN','VE'/
      DATA NO/'N'/
        ITOM=12345

      L=5

      TYPE 9988
9988  FORMAT(/' DONALD ECCLESTONE (C)1979 PRESENTS'//
     *' T H E   H O L Y   T E R R O R   -   T R E K 7'///)
      WRITE(L,*)' TYPE IN A RANDOM NUMBER BETWEEN 1-32767'
        READ(L,*)ITOM
      CALL HARLIE
946   IGO(1)=0
94    CALL FIZBIN
      LAST=0
      NOSTOP=0
      IO=0
      IF(IGO(1).EQ.0)MINES=0
390   DO 128 I=1,4
      IGO(I)=0
      IF(ICHOE(I).EQ.0)GO TO 128
      L=I+4
      WRITE(L,800)
800   FORMAT(' TYPE HELP FOR HELP')
128   CONTINUE
130   IT=0
      L=4
129   IT=IT+1
      IF(IT.GT.4)GO TO 23310
      IBORD=0
      L=IBPSC(IT)+4
      JS=(IT+1)/2
      IS=(IBPSC(IT)+1)/2
      IPUSH(IT)=0
      IPULL(IT)=0
      STATIC(IT)=0
      NANU=0
      IF(ICHOE(IT).NE.1)GO TO 129
      CALL SAURON
234   WRITE(L,10)(INAME(IQ0,IT),IQ0=1,3)
10    FORMAT(/1X,3A4,' COMMAND: '$)
      A=0.0
      B=0.0
      READ(L,920,ERR=3)NA,A,B
920   FORMAT(A2,1X,F5.0,F5.0)
      IGOL(1)=IBLK
      GO TO 4
3     A=0.0
      B=0.0
      CALL ILLDAS(IGOL,L)
4     DO 636 I=1,26
      IF(CMAND(I).EQ.NA)GO TO (585,517,632,15,11,13,330,670,3001,
     *680,310,320,571,666,900,700,200,210,360,352,351,
     *705,350,519,690,695),I
636   CONTINUE
      WRITE(L,800)
      GO TO 234
C                                                                     IN
585   CALL INFO
      GO TO 234
C                                                                     MA
517   CALL GODOT
      GO TO 234
C                                                                     TR
632   CALL SCOTT
      GO TO 234
C                                                                     CO
15    CALL AREX
      GO TO 234
C                                                                     PH
11    CALL MRESS
      GO TO 234
C                                                                     TO
13    CALL CHEKOV
      GO TO 234
C                                                                     CA
670   CALL CANCEL(IBORD)
      GO TO 234
C                                                                     EA
3001  CALL GOLLUM
      GO TO 234
C                                                                     BO
680   CALL ORWELL(IBORD,NUMBO,IRBO,ICBO)
      GO TO 234
C                                                                     YA
310   CALL TRACLC
      GO TO 234
C                                                                     DE
320   CALL DEFLOC
      GO TO 234
C                                                                     HE
571   CALL INST
      GO TO 234
C                                                                     MI
666   IF(LOCKT(IT).NE.1)GO TO 668
      DISTP=RANG(IT)
      IF(A.EQ.0)GO TO 6660
      RANG(IT)=A
      GO TO 6661
6660  WRITE(L,667)
667   FORMAT(' ENTER RANGE AT WHICH TORPEDO IS TO BECOME A MINE - BETWEE
     *N 1.5 AND 10')
      READ(L,*,ERR=66801)RANG(IT)
24    FORMAT(F)
6661  IF(RANG(IT).LT.1.5.OR.RANG(IT).GT.10.)GO TO 6680
      GO TO 234
66801 CALL ILLDAT
6680  CALL OOPS(L)
      RANG(IT)=DISTP
      GO TO 234
668   WRITE(L,6683)
6683  FORMAT(' PLEASE USE TORPEDO COMMAND BEFORE THIS COMMAND')
      GO TO 234
C                                                                     RE
900    CALL BLOCH
      GO TO 234
C                                                                     RA
700   CALL UHURA
      GO TO 234
C                                                                     CE
200   CALL SHADOW(1)
      GO TO 234
C                                                                     AT
210   CALL SHADOW(0)
      GO TO 234
C                                                                     DA
360   I8=0
      WRITE(L,359)
359   FORMAT(' DAMAGE FACTOR-           DECK')
      DO 3611 I=1,33
      I7=MA(IT,I)+1
      IF(I7.EQ.1)GO TO 3611
      I8=1
      WRITE(L,362)I7
362   FORMAT(I3,'-',$)
      CALL FORBIN(JS,I,L,'+')
3611  CONTINUE
      IF(I8.EQ.0)WRITE(L,3612)
3612  FORMAT(' ALL FINE DOWN HERE !!')
      GO TO 234
C                                                                     CR
352   WRITE(L,1450)
1450  FORMAT(' CREW-')
      DO 1440 I=1,4
      IF(ICHOE(I).NE.1)GO TO 1440
      IF(IBPSC(I).EQ.IBPSC(IT))WRITE(L,353)NDEAD(I),
     *(INAME(IQ0,I),IQ0=1,3)
353   FORMAT(1X,I4,' ON ',3A4)
1440  CONTINUE
      DO 1451 I=1,2
      IF(ICHOB(I).EQ.0)GO TO 1451
      IF(IBPSS(I).EQ.IS)WRITE(L,146)NDEAB(I),IBASE(I)
146   FORMAT(1X,I4,' ON STARBASE',I3)
1451   CONTINUE
      IF(IBPOB(7).EQ.0)GO TO 354
      DO 355 I=1,8
      IF(I.EQ.7.OR.ICHOS(I).EQ.0)GO TO 355
      IF(IBPOB(I).EQ.0)GO TO 355
      IF(IBPSB(I).NE.IS)GO TO 355
      WRITE(L,356)IBPOB(I),(IENM2(N,I),N=1,4)
356   FORMAT(1X,I4,' ON ',4A4)
355   CONTINUE
354   IF(NUME(IS).EQ.0)GO TO 234
      DO 357 I=1,LAUNCH
      IF(IBPOE(I).EQ.0)GO TO 357
      IF(IGLER(I).EQ.0)GO TO 357
      IF(IBPSE(I).NE.IS)GO TO 357
      WRITE(L,358)IBPOE(I),I
358   FORMAT(1X,I4,' ON EAGLE ',I2)
357   CONTINUE
      GO TO 234
C                                                                     SP
351   CALL VULCAN
      GO TO 234
C                                                                     EN
705   CALL ECCLE(NANU)
      GO TO 234
C                                                                     NO
350   NOSTOP=1
      GO TO 234
C                                                                     SU
519   IF(IJ(3-IS).EQ.0)GO TO 5190
      DO 170 I=1,4
      IF(ICHOE(I).EQ.0)GO TO 170
      I7=I+4
      WRITE(I7,171)(INAME(IQ0,IT),IQ0=1,3)
171   FORMAT(' THE ',3A4,' WISHES TO SURRENDER')
170   CONTINUE
      ISURR(IT)=1
      WRITE(L,133)
133   FORMAT(' PRESENT YOUR SURRENDER TO THE NEAREST ENEMY VESSEL
     *')
      GO TO 234
5190  WRITE(L,910)(INAME(IQ0,IT),IQ0=1,3)
910   FORMAT(/' THE STARSHIP ',3A4,' IS NOW RAISING THE WHITE FLAG')
      WRITE(L,91)IBASE((IBPSC(IT)+1)/2),(INAME(IQ0,IT),IQ0=1,3)
91    FORMAT(/' STARBASE',I3,' CALLING THE ',3A4,/' DUE TO YOUR COWARDL
     *Y SURRENDER, WE HAVE LOST FACE.'/' FOR THIS ACT YOU ARE DEMOTED TO
     * CESSPOOL CLEANER 4TH CLASS.')
      MAP(IENTR(IT),IENTC(IT))=IBLK
      IF(ISPOT(IT).EQ.1)MAP(IENTR(IT),IENTC(IT))=III
      ICHOE(IT)=0
      IJ(IS)=IJ(IS)-1
      II(IS)=II(IS)-1
      LAST=L
      IF(I1+I2.EQ.0)GO TO 98
      GO TO 129
98    WRITE(LAST,300)
300   FORMAT(' ANOTHER GAME ?')
      READ(LAST,92)NA
92    FORMAT(A1)
      L=LAST
      IF(NO.NE.NA)GO TO 940
      WRITE(LAST,90)
90    FORMAT('1',15('-'),'  KEEP ON TREKKIN`')
      STOP
940   IF(MINES.GT.5)GO TO 946
      DO 945 I=1,4
      IF(IENTR(I).EQ.0)GO TO 945
      MAP(IENTR(I),IENTC(I))=IBLK
      IF(ISPOT(I).EQ.1)MAP(IENTR(I),IENTC(I))=III
945   CONTINUE
      DO 942 I=1,8
      IF(ICHOS(I).EQ.0)GO TO 942
      MAP(IKLNR(I),IKLNC(I))=IBLK
      IF(ISPOK(I).EQ.1)MAP(IKLNR(I),IKLNC(I))=III
942   CONTINUE
      IF(NUMOUT.EQ.0)GO TO 944
      DO 943 I=1,25
      IF(IGLER(I).NE.0)MAP(IGLER(I),IGLEC(I))=IBLK
943   CONTINUE
944   IGO(1)=1
      WRITE(LAST,941)
941   FORMAT(' SAME MAP?')
      READ(LAST,92)NA
      IF(NA.EQ.NO)IGO(1)=0
      GO TO 94
C                                                                     HN
690   CONTINUE
      GO TO 234
C                                                                     VE
695   CONTINUE
      WRITE(L,696)
696   FORMAT(' 7.0 DEVELOPMENTAL')
      GO TO 234
C                                                                     GO
330   IF(IGOL(1).NE.IBLK)GO TO 331
3380  WRITE(L,332)
332   FORMAT(' ENTER COMMAND STRING (M,P,T,B,Y,D,C,H FOR HELP)')
      READ(L,333)IGOL
333   FORMAT(80A1)
331   IF(IGOL(1).NE.'H')GO TO 334
      WRITE(L,335)
335   FORMAT(' ENTER A STRING OF CHARACTERS INDICATING THE SEQUENCE IN W
     *HICH'/' YOU WISH TO EXECUTE AN ACTION - TYPE'/' ''M'' TO MOVE'/' '
     *'P'' TO FIRE PHASERS'/' ''T'' TO FIRE TORPS/DISRUPTORS'/' ''B'' TO
     * INITIATE A BOARDING PARTY'/' ''Y'' TO ENGAGE A TRACTOR BEAM'/' AN
     *D/OR ''D'' TO ENGAGE A DEFLECTOR BEAM. '/' EG. IF YOU WISH TO FIRE
     * A TORPEDO THEN ENGAGE A TRACTOR BEAM THEN INITIATE A'/' BOARDING
     * PARTY THEN MOVE THEN FIRE PHASERS THEN ENGAGE A DEFLECTOR BEAM,'/
     *' TYPE ''TYBMPD''.'/' NOTE THAT YOU DON''T HAVE TO USE ALL THE CHA
     *RACTERS-'/' COMMANDS SUCH AS ''M'' OR ''PT'' OR ''BY'' ARE POSSIBL
     *E.')
      WRITE(L,336)
336   FORMAT(' IF THE FIRST CHARACTER IN THE STRING IS A ''C'','/' THE
     * GO COMMAND IS CANCELLED')
      WRITE(L,337)
337   FORMAT(' OTHER FORMS--'/' EG. B(M)(PT) -IF THE BOARD IS SUCCESSFUL
     *, WE MOVE ELSE WE FIRE PHASERS AND TORPEDOES.'/' THUS THE BACKUS-N
     *AUR FORM OF THE GO-LINE IS-'/' <GO-LINE>::=<GO-CHAR> / <GO-CHAR>(<
     *GO-LINE>)(<GO-LINE>) / <GO-CHAR>(<GO-LINE>) / NIL'/' <GO-CHAR>::=M
     */P/T/B/Y/D'/' CAUTION -THE GO-LINE SHOULD BE 80 CHARACTERS OR LESS
     * IN LENGTH AND SHOULD NOT NEST MORE THAN 6 LEVELS.')
      GO TO 3380
334   IF(IGOL(1).EQ.'C')GO TO 234
      LNA=0
      MOVE=0
      IGO(IT)=2
      LVL=1
      ITAKA(1)=.FALSE.
338   IF(ICHOE(IT).NE.1)GO TO 129
339   LNA=LNA+1
      ISTAT=0
      IF(LNA.LE.80)GO TO 340
343   IF(TWARP(IT).LT.0.5)GO TO 129
      IF(NOMOV(IT).EQ.1)GO TO 129
      IF(MA(IT,29).EQ.9.AND.MA(IT,30).EQ.9)GO TO 129
      CALL ENTEMP(MOVE)
      IF(GAMOVR(LAST))GO TO 98
      GO TO 129
340   IF(IGOL(LNA).EQ.IBLK)GO TO 339
      IF(IGOL(LNA).NE.'(')GO TO 341
      IF(ITAKA(LVL))GO TO 342
      CALL FOSTER(LNA)
      ITAKA(LVL)=.TRUE.
      GO TO 339
342   LVL=LVL+1
      IF(LVL.GT.6)GO TO 343
      ITAKA(LVL)=.FALSE.
      GO TO 339
341   IF(IGOL(LNA).NE.')')GO TO 344
      LVL=LVL-1
      IF(LVL.LE.0)GO TO 343
      ITAKA(LVL)=.FALSE.
      GO TO 339
344   ITAKA(LVL)=.FALSE.
      IF(IGOL(LNA).NE.'M')GO TO 345
      IF(NOMOV(IT).EQ.0)GO TO 346
      WRITE(L,3470)
3470  FORMAT(' MOVEMENT IMPOSSIBLE UNTIL ENGINES RE-ENERGIZED')
      GO TO 339
346   MOVE=MOVE+1
      IF(MOVE.GT.1)GO TO 343
      ITAKA(LVL)=SULU(J)
      GO TO 347
345   IF(IGOL(LNA).NE.'P')GO TO 348
      IF(LOCK(IT).EQ.1)ITAKA(LVL)=SAREK(J)
      DO 5 IAXA=1,2
      ITAKA(LVL)=SAREK(J)
5     CONTINUE
      GO TO 347
348   IF(IGOL(LNA).NE.'T')GO TO 349
      IF(LOCKT(IT).EQ.1)GOTO 347
      DO 6 IXAX=1,4
      ITAKA(LVL)=SPOCK(J)
6     CONTINUE
      GO TO 347
349   IF(IGOL(LNA).NE.'B')GO TO 365
      IF(LOCK(IT).NE.1)GOTO 347
      DO 7 IXAA=1,2
      ITAKA(LVL)=HARPO(IBORD,NUMBO,IRBO,ICBO)
7     CONTINUE
      GO TO 347
365   IF(IGOL(LNA).NE.'Y')GO TO 366
      IF(IPULL(IT).EQ.1)ITAKA(LVL)=LARRY(J)
      GO TO 347
366   IF(IGOL(LNA).NE.'D')GO TO 343
      IF(IPUSH(IT).EQ.1)ITAKA(LVL)=CURLY(J)
347   IF(GAMOVR(LAST))GO TO 98
      GO TO 339
23310 DO 23312 I=1,2
      IF(ICHOB(I).NE.1)GO TO 23312
      IF(NDEAB(I).LE.0)GO TO 23313
      IF(IFIB(I).EQ.1)GO TO 23313
      IF(DFLCB(I).LE.25.)GO TO 23313
      CALL BALOK
23313 IF(IONB(I).NE.0)CALL TALOS
      IF(IGOB(I).EQ.0)DFLCB(I)=DFLCB(I)+10.
      IF(DFLCB(I).GT.300.)DFLCB(I)=300.
      IGOB(I)=0
23312 CONTINUE
      IF(GAMOVR(LAST))GO TO 98
23311 CALL ATACK
      IF(GAMOVR(LAST))GO TO 98
      CALL ENEMY
      DO 301 MI=1,4
      IF(ION(MI).NE.0.OR.ISPOT(MI).NE.0)CALL FINNEY(MI)
301   CONTINUE
      IF(GAMOVR(LAST))GO TO 98
      GO TO 130
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C
      BLOCK DATA
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      DATA IENM1/'K','R','T','G','O','Z','D','A'/
      DATA IENM2/'KLIN','GON ','WARS'
     *,'HIP ','ROMU','LAN ','WARS','HIP ','THOL','IAN ','WARS','HIP ',
     *'GORN',' WAR', 'SHIP','    ','ORIO', 'N PR','IVAT','EER ',
     *'KZIN','TI W','ARSH','IP  ','DOOM',
     *'SDAY',' MAC','HINE','MOON','BASE',' ALP','HA  '/
      DATA IBLK,ISTAR,III,IGLE/' ','*','I','='/
      DATA IEE/'E','P','H','C'/,INAME/'ENTE','RPRI','SE  ','POTE',
     *'MPKI','N   ','HAVO','C   ','    ',
     *'CARN','AGE ','    '/,ISIDE/'FEDE','RATI','ON  ',
     *'KLIN','GON ','    '
     */,IM/'1','2','3','4'/
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                              - GAMOVR -
C
      LOGICAL FUNCTION GAMOVR(LAST)
      COMMON /A/IT,IS,II(2),IJ(2),I3,JS,ISHAK,NOSTOP
      COMMON /B/NDEAB(2),IPHOB(2),IONB(2),IGOB(2),IFIB(2)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /E/PHASR(4),TWARP(4),IPHOT(4),NDEAD(4),ISURR(4)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /K/KILLZ
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /Q/INDUZ,IXRYZ,IMAGZ,IMAGRZ,IABSZ,ISTAZ,IRUNZ,ICLOZ,
     *IONNO,IPLANZ,IDEVZ,IDRIZ,IMAXZ
      COMMON /R/IBPSC(4),IBPSB(8),IBPSS(2),IBPSE(25)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      COMMON /V/IWEBZ
      GAMOVR=.FALSE.
546   DO 762 I=1,8
      IONK(I)=0
      IF(ICHOS(I).NE.0.AND.DFLCK(I).LT.0)CALL HADES
762   CONTINUE
      DO 154 I=1,2
      IF(ICHOB(I).EQ.0)GO TO 154
      J=IBPSS(I)
      IF(DFLCB(I).GE.0)GO TO 155
      CALL EREBUS
      GO TO 1540
155   IF(NDEAB(I).GT.0)GO TO 154
      IF(ICHOB(I).EQ.2)GO TO 154
      CALL CHARON
1540  II(J)=II(J)-1
154   CONTINUE
      DO 134 I=1,4
      IF(ICHOE(I).EQ.0.OR.ICHOE(I).EQ.3)GO TO 134
      IF(DFLCT(I).GE.0)GO TO 2320
      CALL STYX
      IF(I7.EQ.2)GO TO 1341
      GO TO 1340
2320  IF(ICHOE(I).EQ.2)GO TO 134
      IF(NDEAD(I).GT.0)GO TO 134
      CALL BELIAL
1340  J=(IBPSC(I)+1)/2
      IJ(J)=IJ(J)-1
      II(J)=II(J)-1
1341  LAST=IBPSC(I)+4
134   CONTINUE
      IF(IJ(1)+IJ(2).EQ.0)GO TO 98
      IF(ISHAK.EQ.1)GO TO 100
      IF(II(1).NE.0.AND.II(2)+I3.EQ.0)GO TO 139
      IF(II(2).NE.0.AND.II(1)+I3.EQ.0)GO TO 139
      GO TO 100
139   IF(NOSTOP.EQ.2)GO TO 100
      DO 1400 I=1,4
      IF(ICHOE(I).NE.1)GO TO 1400
      J=I+4
      JTK=(IBPSC(I)+1)/2
      WRITE(J,767)(ISIDE(IQ0,JTK),IQ0=1,3),IBASE(JTK),
     *(INAME(IQ0,I),IQ0=1,3)
767   FORMAT(1X,3A4,' STARBASE',I3,' CALLING THE ',3A4,/' CONGRATULATION
     *S, CAPTAIN - OUR SIDE HAS DEFEATED THE ENEMY')
      LAST=J
      IF(NOSTOP.EQ.1)WRITE(J,7650)
7650  FORMAT(' CONTINUING ........')
1400  CONTINUE
      IF(NOSTOP.EQ.0)GO TO 98
      NOSTOP=2
      READ(L,935)AXA
935   FORMAT(A1)
      IF(AXA.NE.'Z')GOTO 100
      IPLANZ=1
      IRUNZ=30
      IDEVZ=180
      IDRIZ=1
      IMAXZ=12
      IONNO=1
      INDUZ=5000
      IXRYZ=5000
      IWEBZ=16
      IMAGZ=15
      IMAGRZ=50
      ICLOZ=1
      IABSZ=60
      ISTAZ=5000
      KILLZ=100
      GO TO 100
98    GAMOVR=.TRUE.
100   ISTAT=0
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - RANDO -
C
      SUBROUTINE RANDO(I,J,K)
        COMMON /TOM/ITOM
        INTEGER*2 ISHRT,ISHIT
        REAL RT
        ISHRT=ITOM
        ISHIT=ITOM
C this is a kludge to make the VMS random num. gen. to work.
        RT=RAN(ISHRT,ISHIT)
        ITOM=IFIX(RT*10000.)
4     TEMP=FLOAT(MOD(ITOM,100))/100.
        I=(K-J+1)*TEMP+J
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                                - HORTA -
C
      SUBROUTINE HORTA(ISTR,ISTC,ITAR,ITAC,RAD,BERNG,IL,AJUST,IGNORE
     *,DIST,KPLOT)
      INTEGER KPLOT(10),INTER(19)
      LOGICAL CYRANO
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUS,MIN,ISTAT,JTK,KOENIG,
     *IGNOR,IO,IGOL(80)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      DATA INTER/'E','P','H','C','K','R','T','G','O','Z','D','A','*',
     *'=','1','2','3','4','I'/
      IF(IO.EQ.1)TYPE 1970,N,ISTR,ISTC,ITAR,ITAC,RAD,BERNG,AJUST,IGNORE
1970  FORMAT(' BEGIN HORTA N=',I5,' ISTR=',I5,' ISTC=',I5,' ITAR=',I5,
     *' ITAC=',I5,' RAD=',F,' BERNG=',F,' AJUST=',F,' IGNORE=',I5)
      IDIST=0
      DISTP=0.0
      I7=ISTR
      I8=ISTC
      IL=1
      J=0
      M=0
      IF(N.GT.2)GO TO 2
      X=DI(ITAC,ITAR,ISTC,ISTR)
      SINA=FLOAT(ITAR-ISTR)/X
      COSA=FLOAT(ITAC-ISTC)/X
      GO TO 3
2     SINA=SIN(BERNG+AJUST)
      COSA=COS(BERNG+AJUST)
3     IF(SINA.NE.0.AND.COSA.NE.0)GO TO 4
      DISTP=DISTP+1.00
      IF(SINA.EQ.0)M=M+1
      IF(COSA.EQ.0)J=J+1
      GO TO 5
4     D1=ABS(FLOAT(J+1)/SINA)
      D2=ABS(FLOAT(M+1)/COSA)
      IF(ABS(D1-D2).LE.0.02)GO TO 60
      IF(D1.GT.D2)GO TO 6
      DISTP=D1
      J=J+1
      GO TO 5
60    DISTP=D1
      J=J+1
      M=M+1
      GO TO 5
6     DISTP=D2
      M=M+1
5     IF(DISTP.GT.RAD)GO TO 7
      I7=ISTR+SIGN(FLOAT(J),SINA)
      I8=ISTC+SIGN(FLOAT(M),COSA)
      IF(CYRANO(I7,I8))GO TO 8
21    NA=MAP(I7,I8)
      IF(NA.NE.INTER(19))GO TO 22
      IF(IGNORE.EQ.1)GO TO 19
      GO TO 15
22    IF(NA.NE.IBLK)GO TO 9
19    IF(N.NE.3)GO TO 13
      IF(IFIX(DISTP).NE.IFIX(DISTP)/2*2)GO TO 3
      IF(IDIST.EQ.IFIX(DISTP))GO TO 3
      KPLOT(IL)=I8
      KPLOT(IL+1)=I7
      IDIST=IFIX(DISTP)
      IL=IL+2
      GO TO 3
9     DO 10 MIN=1,18
      IF(MIN.EQ.IV)GO TO 10
      IF(NA.NE.INTER(MIN))GO TO 10
      IF(MIN.NE.10)RETURN
      IF(I8.NE.IKLNC(6))GO TO 10
      IF(I7.EQ.IKLNR(6))RETURN
10    CONTINUE
      GO TO 13
8     MIN=20
      RETURN
7     MIN=21
      RETURN
13    IF(ISTAT.EQ.11)MAP(I7,I8)=INTER(19)
      IF(N.EQ.5)GO TO 17
      IF(N.NE.1)GO TO 3
      IF(I7.NE.ITAR)GO TO 3
      IF(I8.NE.ITAC)GO TO 3
      MIN=22
      RETURN
15    MIN=19
      RETURN
17    Q=DI(ITAC,ITAR,I8,I7)
      IF(Q.LT.DIST)DIST=Q
      GO TO 3
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - CYRANO -
C
      LOGICAL FUNCTION CYRANO(IR,IC)
      CYRANO=.FALSE.
      IF(IR.LT.1.OR.IR.GT.60.OR.IC.LT.1.OR.IC.GT.60)CYRANO=.TRUE.
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                               - DI -
C
      FUNCTION DI(I,J,K,L)
      DI=SQRT(FLOAT((I-K)**2+(J-L)**2))
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                              - ANG -
C
      FUNCTION ANG(IVR,IVC)
      IF(IVC.NE.0)GO TO 1
      ANG=90.
      GO TO 2
1     ANG=ABS(ATAN(FLOAT(IVR)/FLOAT(IVC)))*180./3.14159265
2     IF(IVC.LT.0.AND.IVR.GE.0)ANG=180.-ANG
      IF(IVC.LT.0.AND.IVR.LT.0)ANG=180.+ANG
      IF(IVC.GE.0.AND.IVR.LT.0)ANG=360.-ANG
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - FOSTER -
C
      SUBROUTINE FOSTER(LNA)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      I7=1
1     LNA=LNA+1
      IF(LNA.GT.80)GO TO 2
      IF(IGOL(LNA).EQ.'(')I7=I7+1
      IF(IGOL(LNA).EQ.')')I7=I7-1
      IF(I7.NE.0)GO TO 1
2     RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                              - IXIF -
C
      INTEGER FUNCTION IXIF(IVV)
      IXIF=IVV
      IF(IVV.LT.0)IXIF=0
      IF(IVV.GT.10)IXIF=10
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                              - LOKI -
C
      LOGICAL FUNCTION LOKI(IP)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      LOKI=.FALSE.
      IF(IP.NE.1)RETURN
      LOKI=.TRUE.
      WRITE(L,1)
1     FORMAT(' WEAPON PREVIOUSLY LOCKED')
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                                - ILLDAT -
C
C WHEN FOROTS VERSION 5 DETECTS AN INPUT ERROR, IT BACKSPACES TO THE
C BEGINNING OF THE ILLEGAL RECORD, AND A BRANCH IS MADE TO THE STATEMENT
C SPECIFIED BY ERR IN THE READ STATEMENT. THIS ROUTINE IS CALLED TO
C CLEAR OUT THIS RECORD SO THE NEXT READ WILL GET DATA FROM THE TTY.
      SUBROUTINE ILLDAT
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(6)
      READ(L,1)
1     FORMAT(1X)
      RETURN
      END

C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                               - OOPS -
C
      SUBROUTINE OOPS(L)
      WRITE(L,1)
1     FORMAT(' ILLEGAL ENTRY - COMMAND CANCELLED')
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                                - ILLDAS -
C
      SUBROUTINE ILLDAS(IGOL,L)
      DIMENSION IGOL(80)
      READ(L,1)IGOL
1     FORMAT(3X,80A1)
      RETURN
      END
