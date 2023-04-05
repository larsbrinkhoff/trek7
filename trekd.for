C=======================================================================
C                                                                      *
C                           TREK7 MODULE D                             *
C                                                                      *
C        SHIP INITIALIZATION AND TERMINATION                           *
C                                                                      *
C        HADES     TOPHET    EREBUS    CHARON    STYX    BELIAL        *
C        HARLIE    FIZBIN    TIMEX     MENDEZ    KZIN                  *
C                                                                      *
C=======================================================================


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                              - HADES -
C
      SUBROUTINE HADES
      COMMON /A/IT,IS,II(2),IJ(2),I3,JS,ISHAK,NDSTOP
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /R/IBPSC(4),IBPSB(8),IBPSS(2),IBPSE(25)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      COMMON /U/LAUNCH,NUMOUT,NUME(2)
      N=I
      J=IBPSB(N)
      IF(J.NE.0)GO TO 54
      I3=I3-1
      GO TO 763
54    II(J)=II(J)-1
      IBPSB(N)=0
763   CALL TOPHET(N)
      IF(N.NE.7)RETURN
766   IV=I7-7
      IVV=I8-7
      IV1=I7+7
      IZA=I8+7
      IF(IV.LE.0)IV=1
      IF(IVV.LE.0)IVV=1
      IF(IV1.GT.60)IV1=60
      IF(IZA.GT.60)IZA=60
      DO 780 N=IV,IV1
      DO 780 J=IVV,IZA
      IF((I8-J)**2+(I7-N)**2.LE.49)MAP(N,J)=IBLK
780   CONTINUE
      DO 769 N=1,8
      IF(ICHOS(N).EQ.0)GO TO 769
      IF(N.EQ.7)GO TO 769
      IF((IKLNC(N)-I8)**2+(IKLNR(N)-I7)**2.GT.49)GO TO 769
      ISPOK(N)=0
      CALL TOPHET(N)
769   CONTINUE
      IF(NUMOUT.EQ.0)GO TO 7690
      DO 7691 N=1,LAUNCH
      IF(IGLER(N).EQ.0)GO TO 7691
      IF((IGLER(N)-I7)**2+(IGLEC(N)-I8)**2.GT.49)GO TO 7691
      DO 540 KOENIG=1,4
      IF(ICHOE(KOENIG).EQ.0)GO TO 540
      J=KOENIG+4
      WRITE(J,7692)N
7692  FORMAT(' EAGLE',I3,' DESTROYED BY EXPLOSION')
540   CONTINUE
      CALL BOOM(N)
7691  CONTINUE
7690  DO 55 KOENIG=1,4
      IF(ICHOE(KOENIG).EQ.0)GO TO 55
      IF((IENTC(KOENIG)-I8)**2+(IENTR(KOENIG)-I7)**2.GT.49)GO TO 55
      DO 56 J=1,4
      IF(ICHOE(J).EQ.0)GO TO 56
      IV=J+4
      WRITE(IV,772)(INAME(IQ0,KOENIG),IQ0=1,3)
772   FORMAT(' THE ',3A4,' HAS BEEN ENGULFED BY THE EXPLOSION'/' FROM TH
     *E DOOMSDAY MACHINE')
56    CONTINUE
      DFLCT(KOENIG)=-10.
55    CONTINUE
      DO 58 N=1,2
      IF(ICHOB(N).EQ.0)GO TO 58
      IF((IBASC(N)-I8)**2+(IBASR(N)-I7)**2.GT.49)GO TO 58
      DFLCB(N)=-10.
      DO 580 J=1,4
      IF(ICHOE(J).EQ.0)GO TO 580
      I7=J+4
      WRITE(I7,59)(ISIDE(IQ0,N),IQ0=1,3),IBASE(N)
59    FORMAT(1X,3A4,' STARBASE',I3,' ENGULFED BY EXPLOSION FROM DOOMSDAY
     * MACHINE')
580   CONTINUE
58    CONTINUE
      DO 57 N=1,5
      IF(LI2R(N).EQ.0)GO TO 57
      IF((LI2R(N)-I7)**2+(LI2C(N)-I8)**2.GT.49)GO TO 57
      LI2R(N)=0
      LI2C(N)=0
57    CONTINUE
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - TOPHET -
C
      SUBROUTINE TOPHET(N)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,NX,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /S/IBPOB(8),IBPOE(25)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      COMMON /V/IWEB(2),IWEBZ,INVIS(4)
      ICHOS(N)=0
      IF(N.EQ.7)GO TO 52
      IF(IBPOB(N).EQ.0)GO TO 52
      IBPOB(N)=0
      IBPOB(7)=IBPOB(7)-1
52    I7=IKLNR(N)
      I8=IKLNC(N)
      IKLNR(N)=0
      IKLNC(N)=0
      IF(ISTAT.EQ.2)GO TO 165
      MAP(I7,I8)=IBLK
      IF(ISPOK(N).EQ.1)MAP(I7,I8)=III
165   DO 53 IV=1,4
      IF(ICHOE(IV).EQ.0)GO TO 53
      J=IV+4
      WRITE(J,102)(IENM2(MIN,N),MIN=1,4)
102   FORMAT(/' SENSORS REPORT THAT AN EXPLOSION HAS PULVERIZED THE ',4A
     *4/'   INTO POWDERED DUST')
53    CONTINUE
      DO 50 J=1,2
      IF(N+4.EQ.IWEB(J))IWEB(J)=0
      IF(N.EQ.J*3)IWEB(J)=0
50    CONTINUE
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - EREBUS -
C
      SUBROUTINE EREBUS
      COMMON /B/NDEAB(2),IPHOB(2),IONB(2),IGOB(2),IFIB(2)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      IF(ISTAT.EQ.2)GO TO 163
163   ICHOB(I)=0
      DFLCB(I)=0
      NDEAB(I)=0
      IONB(I)=0
      DO 156 I7=1,4
      IF(ICHOE(I7).EQ.0)GO TO 156
      N=I7+4
      WRITE(N,157)(ISIDE(IQ0,I),IQ0=1,3),IBASE(I)
157   FORMAT(' SENSORS REPORT THE DESTRUCTION OF ',3A4,' STARBASE',I3)
156   CONTINUE
      IBASE(I)=IBASE(I)+1
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - CHARON -
C
      SUBROUTINE CHARON
      COMMON /B/NDEAB(2),IPHOB(2),IONB(2),IGOB(2),IFIB(2)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      ICHOB(I)=2
      NDEAB(I)=0
      DO 158 I7=1,4
      IF(ICHOE(I7).EQ.0)GO TO 158
      N=I7+4
      WRITE(N,159)(ISIDE(IQ0,I),IQ0=1,3),IBASE(I)
159   FORMAT(' SENSORS DETECT NO LIFE FORMS ABOARD ',3A4,' STARBASE',I3)
158   CONTINUE
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - STYX -
C
      SUBROUTINE STYX
      COMMON /A/IT,IS,II(2),IJ(2),I3,JS,ISHAK,NOSTOP
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /E/PHASR(4),TWARP(4),IPHOT(4),NDEAD(4),ISURR(4)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      COMMON /V/IWEB(2),IWEBZ,INVIS(4)
        COMMON /TOM/ITOM
      CALL RANDO(IV,1,33)
      IVV=I+4
      J=(I+1)/2
2380  WRITE(IVV,238)IBASE(J),(INAME(IQ0,I),IQ0=1,3)
238   FORMAT(' STARBASE',I3,' CALLING ',3A4,'.........'/' AN EXPLOSION H
     *AS OCCURRED IN')
      CALL FORBIN(J,IV,IVV,' ')
      WRITE(IVV,2382)
2382  FORMAT(' AND HAS SPREAD AND ENGULFED YOUR SHIP, DESTROYING YOU'/
     *' AND REDUCING YOUR SHIP TO A LUMP OF MOLTEN SLAG')
      IF(I.EQ.IT)MIN=1
      IF(ISTAT.EQ.2)GO TO 164
      MAP(IENTR(I),IENTC(I))=IBLK
      IF(ISPOT(I).EQ.1)MAP(IENTR(I),IENTC(I))=III
164   DFLCT(I)=0
      NDEAD(I)=0
      IENTR(I)=0
      IENTC(I)=0
      ION(I)=0
      ISPOT(I)=0
      DO 135 I7=1,4
      IF(I7.EQ.I)GO TO 135
      IV=I7+4
      IF(ICHOE(I7).NE.0)WRITE(IV,136)(INAME(IQ0,I),IQ0=1,3)
136   FORMAT(' SENSORS REPORT THE DESTRUCTION OF THE ',3A4)
135   CONTINUE
      IF(IWEB(1).EQ.I)IWEB(1)=0
      IF(IWEB(2).EQ.I)IWEB(2)=0
      I7=ICHOE(I)
      ICHOE(I)=3
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - BELIAL -
C
      SUBROUTINE BELIAL
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /E/PHASR(4),TWARP(4),IPHOT(4),NDEAD(4),ISURR(4)
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
      IVV=I+4
      WRITE(IVV,615)
615   FORMAT(' CREW REMAINING IS ZERO'/9X,'THUS THERE IS NO ONE LEFT TO
     * CONTROL THE SHIP')
      NDEAD(I)=0
      ICHOE(I)=2
      DO 137 I7=1,4
      IF(I7.EQ.I)GO TO 137
      IV=I7+4
      IF(ICHOE(I7).NE.0)WRITE(IV,136)(INAME(IQ0,I),IQ0=1,3)
136   FORMAT(' SENSORS DETECT NO LIFE FORMS ABOARD THE ',3A4)
137   CONTINUE
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - HARLIE -
C
      SUBROUTINE HARLIE
      COMMON /W/IDEX(2,33,20)
C      OPEN(UNIT=1,DEVICE='DRA0:',ACCESS='RANDIN',MODE='ASCII',FILE='KIRK.
C     *DAT',DIRECTORY='050033.D.WORK',RECORD SIZE=80)
C      OPEN(UNIT=2,DEVICE='DRA0:',ACCESS='RANDIN',MODE='ASCII',FILE='KOLOTH
C     *.DAT',DIRECTORY='050033.D.WORK',RECORD SIZE=80)
      OPEN(UNIT=1,TYPE='OLD',ACCESS='DIRECT',
     *FORM='FORMATTED',
     *RECORDTYPE='FIXED',RECORDSIZE=80,readonly,
     *ORGANIZATION='RELATIVE',NAME='SYS$USERS:KIRK.DAT')
      OPEN(UNIT=2,TYPE='OLD',ACCESS='DIRECT',
     *FORM='FORMATTED',
     *RECORDTYPE='FIXED',RECORDSIZE=80,readonly,
     *ORGANIZATION='RELATIVE',NAME='SYS$USERS:KOLOTH.DAT')
      DO 1 I=1,33
      READ(1'I,10)(IDEX(1,I,J),J=1,20)
1     READ(2'I,10)(IDEX(2,I,J),J=1,20)
10    FORMAT(20A4)
c close the files so that some other joe might play
        close(1)
        close(2)
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - FIZBIN -
C
      SUBROUTINE FIZBIN
      INTEGER LAP(3600),LA(132),LK(56),IGOM(8)
      EQUIVALENCE (LAP,MAP),(LA,MA),(LK,K)
c***
        integer itom2,idev(2)
        integer*2 icnt
c used by vax version
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
94    CALL RANDO(IBASE(1),1,17)
      CALL RANDO(IBASE(2),1,17)
      IF(IBASE(1).EQ.IBASE(2))GO TO 94
      CALL TIMEX
        write      LAUNCH=0
      ISHAK=0
      NUMOUT=0
      DO 8910 I=1,25
      IGLER(I)=0
      IGLEC(I)=0
      IBPSE(I)=0
8910  IBPOE(I)=0
      ISTAT=0
      MIN=0
105   WRITE(L,104)
104   FORMAT(' ENTER THE MANUALLY-OPERATED SHIPS YOU WISH BY ENTERING A
     * STRING CONTAINING-'/' ''E'' FOR ENTERPRISE, ''P'' FOR POTEMPKIN (
     *FEDERATION),'/' ''H'' FOR HAVOC, ''C'' FOR CARNAGE (KLINGON)')
      READ(L,332)(IGOM(I),I=1,4)
332   FORMAT(4A1)
107   DO 108 I=1,4
      ICHOE(I)=0
      DO 108 J=1,4
      IF(IGOM(J).NE.IEE(I))GO TO 108
      ICHOE(I)=1
      ISTAT=ISTAT+1
108   CONTINUE
      IF(ISTAT.EQ.0)GO TO 105
      IF(ISTAT.EQ.1.AND.ICHOE(1).EQ.1)GO TO 111
      WRITE(L,109)
c109   FORMAT(' WHEN I STOP TYPING, HIT %C AND ENTER THE FOLLOWING COMMAN
c     *DS-'/' .ASSIGN TTYAA 5'/' .ASS TTYBB 6'/' .ASS TTYCC 7'/' .ASS TTY
c     *DD 8'/' .CONTINUE'/' <CR>'//' (WHERE AA, BB, CC, DD ARE THE TTY''S
c     * CONTROLLING THE ENTERPRISE,'/' POTEMPKIN, HAVOC AND CARNAGE RESPE
c     *CTIVELY)'/' (IF YOU''RE NOT PLAYING SOME OF THE SHIPS, DON''T BOTH
c     *ER'/' ENTERING THEIR ASSIGN COMMANDS)'/' (IF YOU''VE HAD THE FORES
c     *IGHT TO DO ALL THIS, JUST HIT <CR>)')
c      READ(L,92)NA
c92    FORMAT(A1)

109     format(' It is time to assign terminals to TREK7. To do
     *this you need to know'/' the device name of the terminals
     *that you will be using.'/' If you do not know this then
     *stop the game and find out.'/' To do this type "show term"
     *, the first piece of information is the name of our terminal'/'
     *, for example "TUB1:". Find out the name of all the terminals
     *'/' and then restart the game.')
c***vax version
c
        icnt=5
        do 926 itom2=1,4
           if(igom(itom2).eq.'E')goto 921
           if(igom(itom2).eq.'P')goto 922
           if(igom(itom2).eq.'H')goto 923
           if(igom(itom2).eq.'C')goto 924
        goto 926
c
c
921     write(L,9211)
9211    format(' IF you want the Enterprise to play off of this
     *terminal then type a blank.'/' Do not type in the name of
     *this terminal, this causes the game to die.')
        read(L,9212)(idev(j),j=1,2)
9212    format(2(A4))
        if (idev(1).eq.' ')goto 926
        call assign(5,idev,icnt)
        goto 926
c
c
922     write(L,9221)
9221    format(' The Potempkin is assigned to terminal ',$)
        read(L,9222)(idev(j),j=1,2)
9222    format(2(a4))
        call assign(6,idev,icnt)
        goto 926
c
c
923     write(5,9231)
9231    format(' The Havoc is assigned to terminal ',$)
        read(L,9232)(idev(j),j=1,2)
9232    format(2(a4))
        call assign(7,idev,icnt)
        goto 926
c
c
924     write(5,9241)
9241    format(' The Carnage is assigned to terminal ',$)
        read(L,9242)(idev(j),j=1,2)
9242    format(2(a4))
        call assign(8,idev,icnt)
c
c
926   continue
c
c
c
c***
111   DO 110 I=1,4
      IF(ICHOE(I).EQ.0)GO TO 110
      IBPSC(I)=I
      ISPOT(I)=0
      ION(I)=0
      ZAP(I)=0
      INVIS(I)=0
      TWARP(I)=10.
      LOCK(I)=-1
      LOCKT(I)=-1
      ICOIL(I)=0
      WARP(I)=0.
      NOMOV(I)=0
      IPHOT(I)=12
      PHASR(I)=6000.
      DFLCT(I)=100.
      ISURR(I)=0
      RANG(I)=10.
      NOMAP(I)=15
      MANUM(I)=0
      ITEMP(I)=4000
      NDEAD(I)=430
      NOMOV(I)=0
      CALL RANDO(IENTR(I),1,60)
      CALL RANDO(IENTC(I),1,60)
      LI2(I)=0
110   CONTINUE

      I3=0
      DO 120 I=1,2
      II(I)=0
      NUME(I)=0
      ICHOB(I)=0
      IONB(I)=0
      IBPSS(I)=I
      NDEAB(I)=1000
      IPHOB(I)=20
      IWEB(I)=0
      IGOB(I)=0
      IFIB(I)=0
120   DFLCB(I)=300.
      INDUZ=0
      IXRYZ=0
      IWEBZ=0
      ISTAZ=0
      KILLR=1
      KILLG=1
      KILLD=1
      IONNO=0
112   CALL RANDO(I7,1,4)
      IF(ICHOE(I7).EQ.0)GO TO 112
      L=I7+4
      DO 113 I=1,4
      IF(I.EQ.I7.OR.ICHOE(I).EQ.0)GO TO 113
      I8=I+4
      WRITE(I8,114)(INAME(IQ0,I7),IQ0=1,3)
114   FORMAT(' PLEASE WAIT WHILE THE ',3A4,' CHOOSES THE ENEMIES')
113   CONTINUE
      WRITE(L,751)
751   FORMAT(' PICK THE OPPONENT(S) YOU WISH TO FIGHT BY TYPING IN A STR
     *ING OF LETTERS-'/' EACH LETTER CORRESPONDING TO THE OPPONENT YOU W
     *ISH TO BATTLE.'/' TYPE ''K'' FOR KLINGONS, ''R'' FOR ROMULANS'/' '
     *'T'' FOR THOLIANS, ''G'' FOR GORNS'/' ''O'' FOR ORIONS, ''Z'' FOR
     * KZINTI'/' ''D'' FOR A DOOMSDAY MACHINE'/' AND/OR ''A'' FOR MOONBA
     *SE ALPHA')
      READ(L,752)(IGOM(I),I=1,8)
752   FORMAT(8A1)
      DO 7531 I=1,8
      DFLCK(I)=100.
      IBPOB(I)=0
      IBPSB(I)=0
      ICHOS(I)=0
      ISPOK(I)=0
      IONK(I)=0
      IGOCO(I)=1
      DO 753 J=1,8
      IF(IGOM(J).NE.IENM1(I))GO TO 753
      ICHOS(I)=1
753   CONTINUE
7531  CONTINUE
      DFLCK(8)=70.
      IF(ICHOS(6).EQ.1)CALL KZIN
      IF(IGO(1).EQ.1)GO TO 1260
      DO 501 I=1,3600
501   LAP(I)=IBLK
      CALL RANDO(IV,0,10)
      IF(IV.EQ.0)GO TO 1260
      DO 647 N=1,IV
648   CALL RANDO(IVV,4,11)
      CALL RANDO(MIN,1,60)
      CALL RANDO(J,1,60)
      I7=MIN+IVV
      I8=J+IVV
      IF(I7.GT.60)GO TO 648
      IF(I8.GT.60)GO TO 648
      DO 649 IVV=J,I8
      DO 649 I=MIN,I7
649   MAP(I,IVV)=III
647   CONTINUE
1260  II(1)=ICHOE(1)+ICHOE(2)
      II(2)=ICHOE(3)+ICHOE(4)
      IJ(1)=II(1)
      IJ(2)=II(2)
      DO 115 I=1,8
115   IF(ICHOS(I).EQ.1)I3=I3+1
      DO 119 IVV=1,2
      J=3-IVV
      IF(II(IVV).EQ.0.OR.II(J).NE.0)GO TO 119
      CALL MENDEZ(J)
119   CONTINUE
      DO 116 IVV=1,2
      IF(II(IVV).EQ.0)GO TO 116
      IF(ICHOB(IVV).NE.0)GO TO 116
      IF(II(3-IVV).NE.ICHOB(3-IVV))GO TO 117
      IF(3*II(IVV).GT.I3)GO TO 116
117   CALL MENDEZ(IVV)
116   CONTINUE
      IF(II(2).EQ.0.AND.I3.EQ.0)ISHAK=1
      IF(II(1).EQ.0.AND.I3.EQ.0)ISHAK=1
      IF(ISHAK.EQ.0)GO TO 126
      DO 122 J=1,4
      I=J+4
      IF(ICHOE(J).EQ.1)WRITE(I,6100)
6100  FORMAT(' BEGINNING SHAKEDOWN CRUISE')
122   CONTINUE
126   IF(IGO(1).EQ.1)GO TO 5020
      WRITE(L,610)
610   FORMAT(' HOW MANY STARS DO YOU WISH IN YOUR 60 X 60 FIELD - BETWEE
     *N 0 AND 500')
      GO TO 655
1262  CALL ILLDAT
      GO TO 126
655   READ(L,24,ERR=1262)DISTP
1261  IF(DISTP.GT.1500.)GO TO 126
24    FORMAT(F)
123   I7=IFIX(DISTP)
      IF(I7.LT.0)GO TO 126
      ISTAT=0
      IF(I7.EQ.0)GO TO 5022
      DO 502 I=1,I7
      CALL RANDO(IV,1,60)
      CALL RANDO(IVV,1,60)
      IF(ISTAT.GE.5)GO TO 502
      CALL RANDO(J,1,100)
      IF(J.GT.5)GO TO 502
      ISTAT=ISTAT+1
      LI2R(ISTAT)=IV
      LI2C(ISTAT)=IVV
502   MAP(IV,IVV)=ISTAR
5022  IF(ISTAT.GE.5)GO TO 5020
      DO 5021 I=ISTAT+1,5
      LI2R(I)=0
5021  LI2C(I)=0
5020  DO 125 I=1,4
      IF(ICHOE(I).EQ.0)GO TO 125
      IF(MAP(IENTR(I),IENTC(I)).NE.III)GO TO 1250
      ISPOT(I)=1
      ION(I)=1
125   CONTINUE
      DO 754 I=1,8
      IKLNR(I)=0
      IKLNC(I)=0
      IF(ICHOS(I).NE.1)GO TO 754
7501  CALL RANDO(I7,2,59)
      CALL RANDO(I8,2,59)
      NA=MAP(I7,I8)
      IF(NA.NE.IBLK.AND.NA.NE.ISTAR.AND.NA.NE.III)GO TO 7501
      IF(I.EQ.8.AND.NA.EQ.III)GO TO 7501
      IKLNR(I)=I7
      IKLNC(I)=I8
7502  IF(I.NE.6.OR.ICHOS(6).EQ.0)GO TO 750
      CALL NIVEN
      GO TO 754
750   CALL ASIMOV(NA,I)
151   MAP(I7,I8)=IENM1(I)
754   CONTINUE
      IF(ICHOS(2).EQ.0)GO TO 890
      DO 127 I=1,4
      IF(ICHOE(I).EQ.0)GO TO 127
      IF(DI(IKLNC(2),IKLNR(2),IENTC(I),IENTR(I)).GE.5)GO TO 127
      INVIS(I)=1
127   CONTINUE
890   IF(IARMZ(5).EQ.0)GO TO 89
      IF(ICHOS(6).EQ.0)GO TO 89
      DO 891 I=1,4
      IF(ICHOE(I).EQ.0)GO TO 891
      IF(DI(IKLNC(6),IKLNR(6),IENTC(I),IENTR(I)).GE.ICLOZ)GO TO 891
      INVIZ(I)=1
891   CONTINUE
89    ISTAT=0
      DO 170 I=1,8
      KODE(1,I)=0
170   KODE(2,I)=0
      DO 171 I=1,2
      CALL RANDO(IV,0,2)
      IF(IV.EQ.0)GO TO 171
      CALL RANDO(I8,1,8)
      KODE(I,I8)=1
      CALL RANDO(JTK,0,2)
      IF(JTK.NE.0)GO TO 171
      IVV=2*I
      DO 1710 MIN=IVV-1,IVV
      IF(ICHOE(MIN).EQ.0)GO TO 1710
      J=MIN+4
      WRITE(J,172)I8
172   FORMAT(' INTELLIGENCE REPORTS THAT THE ENEMY HAVE BROKEN CODE',I2)
1710  CONTINUE
171   CONTINUE
      DO 118 I=1,56
      LA(I)=0
118   LK(I)=0
      DO 747 I=57,132
747   LA(I)=0
      IF(ICHOS(8).NE.1)GO TO 8900
      CALL LEGUIN(0,0)
8900  RETURN
      END


C
C-   TAKES A LICKIN' BUT KEEPS ON TICKIN'
C
C-                  TIMEX
C
      SUBROUTINE TIMEX
      IMPLICIT INTEGER (A-Z)
      DIMENSION I(2),MON(12),F(12)
C      DATA MON/-28580569024,-30711668672,-26969825216,-33349361600,
C     *-26969595840,-28496682944,-28496748480,-33328750528,-23731888064,
C     *-25887629248,-26374102976,-31785377728/
C      DATA F/0,3,3,6,1,4,6,2,5,0,3,5/
C      CALL DATE(I)
C      CALL TIME(D,A)
      RETURN
C      CALL SETRAN(M)
      DECODE(2,4,D)M
4     FORMAT(I2)
      IF(M.LT.10.OR.M.GT.16)RETURN
      DECODE(9,1,I)D,A,Y
1     FORMAT(I2,1X,A3,1X,I2)
      Y=Y+1900
      DO 2 M=1,12
      IF(A.EQ.MON(M))GO TO 3
2     CONTINUE
3     IF(M.GE.4.AND.M.LE.9)RETURN
      LL=0
      Y=Y-400*(Y/400)
      A=Y+(Y+3)/4-(Y-1)/100
      IF(Y.EQ.(Y/4)*4.AND.M.GE.3)LL=1
      A=A+D+LL+F(M)
      A=A-(A/7)*7
      IF(A.EQ.1.OR.A.EQ.22)RETURN
      L=5
      CALL RANDO(A,1,7)
      GO TO (9,10,12,14,16,18,20),A
14    WRITE(L,15)
15    FORMAT(' AS PASSWORD ENTER NAME OF THE DISCOVERER OF THE SPACE WAR
     *P')
      GO TO 8
12    WRITE(L,13)
13    FORMAT(' AS PASSWORD ENTER NAME OF ENTERPRISE''S 1ST CAPTAIN')
      GO TO 8
10    WRITE(L,11)
11    FORMAT(' AS PASSWORD ENTER KIRK''S MIDDLE NAME')
      GO TO 8
9     WRITE(L,6)
6     FORMAT(' AS PASSWORD ENTER NAMES OF SIGNERS OF ORGANIAN PEACE TREA
     *Y')
      GO TO 8
16    WRITE(L,17)
17    FORMAT(' AS PASSWORD ENTER MUDD''S FULL NAME')
      GO TO 8
18    WRITE(L,19)
19    FORMAT(' AS PASSWORD ENTER NAME OF SHIP WITH STAR FLEET ID NUMBER
     * NCC-3810')
      GO TO 8
20    WRITE(L,21)
21    FORMAT(' AS PASSWORD ENTER WEIGHT OF ENTERPRISE IN CARATS')
8     READ(L,7)Y,A,D,M,LL,I
7     FORMAT(7A5)
      WRITE(5,5)
5     FORMAT(' SORRY - INCORRECT PASSWORD-'/' IN ACCORDANCE WITH THE UNI
     *VERSITY COMPUTING COUNCIL''S POLICY'/' (SEE HLP:RESTRI.MEM), TREK7
     * CANNOT BE ACCESSED ON WEEKDAYS'/' BETWEEN THE HOURS OF 1000 AND 1
     *700.'/' COME BACK TONIGHT AND PLAY IT WITHOUT THESE RESTRICTIONS.'
     *)
      STOP
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                            - MENDEZ -
C
      SUBROUTINE MENDEZ(J)
      COMMON /A/IT,IS,II(2),IJ(2),I3,JS,ISHAK,NOSTOP
      COMMON /B/NDEAB(2),IPHOB(2),IONB(2),IGOB(2),IFIB(2)
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(8),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /T/ICHOE(4),ICHOS(8),ICHOB(2)
        COMMON /TOM/ITOM
      I=1
      N=4
      IF(II(1).EQ.0)I=3
      IF(II(2).EQ.0)N=2
      IF(J.EQ.2.AND.ICHOB(1).EQ.1)I=3
      DO 1 MIN=I,N
      IF(ICHOE(MIN).EQ.0)GO TO 1
      I8=MIN+4
      WRITE(I8,2)(ISIDE(IQ0,J),IQ0=1,3)
2     FORMAT(' DO YOU WISH A ',3A4,' STARBASE?')
      READ(I8,3)NA
3     FORMAT(A1)
      IF(NA.NE.'Y')GO TO 4
1     CONTINUE
      ICHOB(J)=1
      CALL RANDO(IBASR(J),1,60)
      CALL RANDO(IBASC(J),1,60)
      IF(MAP(IBASR(J),IBASC(J)).EQ.III)IONB(J)=1
      MAP(IBASR(J),IBASC(J))=ISTAR
      II(J)=II(J)+1
      RETURN
4     DO 5 IV=1,4
      IF(ICHOE(IV).EQ.0)GO TO 5
      IF(IV.EQ.MIN)GO TO 5
      I8=IV+4
      WRITE(I8,6)(ISIDE(IQ0,J),IQ0=1,3),(INAME(IQ0,MIN),IQ0=1,3)
6     FORMAT(1X,3A4,' STARBASE VETOED BY ',3A4)
5     CONTINUE
      RETURN
      END


C          -- DONALD ECCLESTONE SUBPROGRAM FOR  T R E K 7 --
C
C                             - KZIN -
C
      SUBROUTINE KZIN
      COMMON /C/L,A,B,I,NA,IV,I7,I8,N,DISTP,AJUST,MIN,ISTAT,JTK,KOENIG,
     *IGNORE,IO,IGOL(80)
      COMMON /D/DFLCT(4),DFLCK(8),DFLCB(2)
      COMMON /I/IONK(8),ISPOK(8),ION(4),ISPOT(4)
      COMMON /K/IWHO(8),DIST(8),KILLZ,KILLR,KILLD,KILLG,IGOCO(8)
      COMMON /L/IENTR(4),IENTC(4),IKLNR(8),IKLNC(5),IGLER(25),IGLEC(25),
     *IBASR(2),IBASC(2),LI2(4),LI2R(5),LI2C(5),IGO(4),MINES
      COMMON /M/MAP(60,60),IBLK,IENM1(8),IEE(4),IGLE,IM(4),III,ISTAR
      COMMON /N/INAME(3,4),IENM2(4,8),ISIDE(3,2),IBASE(2)
      COMMON /Q/IARMZ(6),INDUZ,IXRYZ,IMAGZ,IMAGRZ,IABSZ,ISTAZ,INVIZ(4),
     *ICLOZ,IONNO,IPLANZ,IRUNZ,IDEVZ,IDRIZ,IMAXZ
      COMMON /S/IBPOB(8),IBPOE(25)
      COMMON /V/IWEB(2),IWEBZ,INVIS(4)
1     IF(IBPOB(6).NE.0)RETURN
      WRITE(L,2)
2     FORMAT(' THE KZINTI ARE A SPECIAL ENEMY- THEY ARE PLAYER PROGRAMMA
     *BLE.'/' THUS THE PLAYER ENTERS PARAMETERS REGARDING THE ENEMY STRE
     *NGTH'/' AND STRATEGY.')
7     WRITE(L,3)
3     FORMAT(//' TYPE OF STRATEGY -- ENTER A NUMBER-'/
     *' ''1'' FOR ATTACK/RETREAT'/
     *' ''2'' FOR KAMIKAZE'/
     *' ''3'' FOR CLOSE PASSES (EG. ORIONS)')
      N=1
      READ(L,9,ERR=73)DISTP
      IPLANZ=IFIX(DISTP)
      IF(IPLANZ.LT.1.OR.IPLANZ.GT.3)GO TO 7
      IF(IPLANZ.NE.1)GO TO 15
10    WRITE(L,8)
8     FORMAT(' ENTER A NUMBER REPRESENTING BY HOW MUCH YOUR
     *'/' DEFLECTORS MUST EXCEED THE KZINTIS'' BEFORE THE KZINTI START'
     */'TO RETREAT.'/' EG. 30 MEANS THE KZINTI RETREAT WHEN YOUR
     * DEFLECTORS ARE'/5X,' GREATER THAN THEIRS BY AT LEAST 30.'/'
     * EG. -30 MEANS THE KZINTI RETREAT UNLESS THEY EXCEED YOUR
     *'/5X,' DEFLECTORS BY AT LEAST 30.')
      N=2
      READ(L,9,ERR=73)DISTP
9     FORMAT(F)
      IRUNZ=IFIX(DISTP)
131   WRITE(L,130)
130   FORMAT (' ENTER A NUMBER (IN DEGREES BETWEEN 0 AND 180) REPRESENTI
     *NG'/' THE MAXIMUM ANGLE THE KZINTIS'' PATH MAY DEVIATE FROM A PATH
     * HEADING'/' STRAIGHT TOWARDS YOU (ATTACK) OR STRAIGHT AWAY FROM YO
     *U (RETREAT).'/' EG. 0 MEANS THEY WILL HEAD STRAIGHT FOR YOU WHEN T
     *HEY ATTACK'/5X,'OR STRAIGHT AWAY FROM YOU WHEN THE RETREAT.'/' EG.
     * 90 MEANS THEIR PATH MAY RANDOMLY DEVIATE OFF TO THE SIDE A MAXIMU
     *M'/5X,'OF 90 DEGREES FROM HEADING STAIGHT AT YOU OR STAIGHT AWAY F
     *ROM YOU')
      N=3
      READ(L,9,ERR=73)DISTP
      IDEVZ=IFIX(DISTP)
      IF(IDEVZ.LT.0.OR.IDEVZ.GT.180)GO TO 131
15    WRITE(L,16)
16    FORMAT(' TYPE OF ENGINES -- ENTER A NUMBER-'/
     *' ''1'' FOR IONIC DRIVE ENGINES'/
     *' ''2'' FOR WARP DRIVE ENGINES')
      N=4
      READ(L,9,ERR=73)DISTP
      IDRIZ=IFIX(DISTP)
      IF(IDRIZ.LT.1.OR.IDRIZ.GT.2)GO TO 15
21    WRITE(L,20)
20    FORMAT(' ENTER MAXIMUM WARP -- BETWEEN 2 AND 12')
      N=5
      READ(L,9,ERR=73)DISTP
      IMAXZ=IFIX(DISTP)
      IF(IMAXZ.LT.2.OR.IMAXZ.GT.12)GO TO 21
      WRITE(L,25)
25    FORMAT(' DO YOU WANT THE KZINTIS'' SPEED TO BE UNAFFECTED BY ION S
     *TORMS?')
      N=6
      READ(L,11,ERR=73)NA
11    FORMAT(A1)
      IF(NA.EQ.'Y')IONNO=1
30    WRITE(L,31)
31    FORMAT(' ARMAMENT -- ENTER A STRING OF DIGITS-'/
     *' ''1'' FOR INDUCTION BEAMS'/
     *' ''2'' FOR X-RAY LASER CANNON'/
     *' ''3'' FOR WEB (LIKE THOLIAN''S)'/
     *' ''4'' FOR DECOY IMAGES'/
     *' ''5'' FOR CLOAKING DEVICE'/
     *' AND/OR ''6'' FOR STASIS FIELD')
      READ(L,32,ERR=73)(IONK(I7),I7=1,6)
32    FORMAT(6I1)
      DO 33 I8=1,6
      IARMZ(I8)=0
      DO 33 I7=1,6
      IF(IONK(I7).EQ.I8)IARMZ(I8)=1
33    CONTINUE
      DO 330 I8=1,6
330   IONK(I8)=0
      IF(IARMZ(1).EQ.0)GO TO 40
35    WRITE(L,34)
34    FORMAT(' ENTER INDUCTION BEAM STRENGTH IN PHASER UNITS')
      N=7
      READ(L,9,ERR=73)DISTP
      IF(DISTP.GE.250.)GO TO 400
      WRITE(L,401)
401   FORMAT(' NUMBER TOO SMALL')
      GO TO 35
400   INDUZ=IFIX(DISTP)/10
40    IF(IARMZ(2).EQ.0)GO TO 70
71    WRITE(L,72)
72    FORMAT(' ENTER X-RAY CANNON STRENGTH IN PHASER UNITS')
      N=8
      READ(L,9,ERR=73)DISTP
      IF(DISTP.GE.250.)GO TO 700
      WRITE(L,401)
      GO TO 71
700   IXRYZ=IFIX(DISTP)/10
70    IF(IARMZ(3).EQ.0)GO TO 50
41    WRITE(L,42)
42    FORMAT(' ENTER RANGE OF WEB')
      N=9
      READ(L,9,ERR=73)DISTP
      IWEBZ=IFIX(DISTP)
      IF(IWEBZ.LE.0)GO TO 41
50    IF(IARMZ(4).EQ.0)GO TO 53
54    WRITE(L,55)
55    FORMAT(' ENTER NUMBER OF FALSE IMAGES TO BE GENERATED- AT MOST 1
     *5')
      N=10
      READ(L,9,ERR=73)DISTP
      IMAGZ=IFIX(DISTP)
      IF(IMAGZ.LT.0.OR.IMAGZ.GT.15)GO TO 54
58    WRITE(L,57)
57    FORMAT(' ENTER RADIUS WITHIN WHICH THE IMAGES ARE TO BE GENERATED
     *')
      N=11
      READ(L,9,ERR=73)DISTP
      IMAGRZ=IFIX(DISTP)
      IF(IMAGRZ.LT.1.OR.IMAGRZ.GT.50)GO TO 58
53    IF(IARMZ(5).EQ.0)GO TO 60
51    WRITE(L,52)
52    FORMAT(' ENTER RANGE AT WHICH ENTERPRISE CAN DETECT THE KZINTI SH
     *IP'/' THROUGH THE CLOAKING DEVICE')
      N=12
      READ(L,9,ERR=73)DISTP
      ICLOZ=IFIX(DISTP)
      IF(ICLOZ.LE.0)GO TO 51
60    IF(IARMZ(6).EQ.0)GO TO 29
61    WRITE(L,62)
62    FORMAT(' ENTER RANGE OF STASIS FIELD')
      N=13
      READ(L,9,ERR=73)DISTP
      IABSZ=IFIX(DISTP)
      IF(IABSZ.LE.0)GO TO 61
64    WRITE(L,63)
63    FORMAT( ' ENTER STRENGTH OF STASIS FIELD IN EQUIVALENT PHASER UNIT
     *S PER TURN')
      N=14
      READ(L,9,ERR=73)DISTP
      ISTAZ=IFIX(DISTP)
      IF(ISTAZ.LT.1)GO TO 64
29    KILLZ=100
      WRITE(L,27)
27    FORMAT(' IF YOU WISH THE KZINTI TO BE NEUTRAL UNTIL FIRED AT'/' O
     *R UNTIL YOU COME WITHIN A CERTAIN RANGE, ENTER THE RANGE AT WHICH'
     */' THEY BECOME HOSTILE.'/' IF NOT, TYPE ''100''.')
      N=15
      READ(L,9,ERR=73)DISTP
      KILLZ=IFIX(DISTP)
      IF(KILLZ.LE.0.OR.KILLZ.GT.100)GO TO 29
280   IF(IARMZ(1).EQ.1.OR.IARMZ(2).EQ.1)GO TO 28
      IARMZ(1)=1
      IARMZ(2)=1
      INDUZ=90
      IXRYZ=90
28    RETURN
73    CALL ILLDAT
      GO TO (7,10,131,15,21,30,35,71,41,54,58,51,61,64,280),N
      END
