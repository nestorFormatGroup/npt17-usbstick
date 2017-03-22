!******************************************************************************
!                                                                             |
!          SUBPROGRAM DOCUMENTATION BLOCK -- EXMOISG                          | 
!                                                                             |
!     PURPOSE: PARAMETERIZATION OF MIXED PHASE CLOUD AND PRECIPITATION 
!              MICROPHYSICS  
!                                                                             |
!     HISTORY                                                                 |
!                                                                             |
!       ORIGINAL CODING BY JON REISNER (now at LANL), CONCEPTS BY ROY         |
!         RASMUSSEN AND ROELOF BRUINTJES, NCAR.                               |
!                                                                             | 
!                                                                             |
!       8 JAN 96: RECEIVED CODE USED IN RUC/MAPS DESCENDED FROM MM5 TEST      |
!         VERSION                                                             | 
!         INCLUDING EXPLICIT PREDICTION OF NUMBER CONCENTRATIONS FOR SNOW     |
!         AND GRAUPEL (MM5 level 5, so called Reisner 3 scheme) RECEIVED FROM |
!         ROELOF BRUINTJES, NCAR.                                             | 
!                                                                             |
!       1996: NUMEROUS MODS TO ADAPT SCHEME TO MM5 LEVEL 4                    |
!         (no explicit prediction                                             |
!         of number concentration of snow and graupel)                        |
!         FOR USE IN MAPS/RUC.  SCHEME FIRST INTRODUCED INTO REAL-TIME MAPS   |
!         CYCLE AT FSL EARLY 1997.                                            |
!                                                                             | 
!       1997: SEVERAL BUG FIXES, ENHANCEMENTS ADDED.  SIGNIFICANT PROBLEMS    |
!             CONTINUED WITH EXCESSIVE GRAUPEL PRODUCTION AT LEVELS JUST      |
!             ABOVE FREEZING AND AT COLD TEMPERATURES (< -30C).               |
!                                                                             |
!       NCAR-FSL COLLABORATION PRODUCED VARIOUS FURTHER BUG FIXES,            |
!         ENHANCEMENTS, 1998-9, BUT NONE OF THESE MADE IT INTO THE REAL-TIME  |
!         MAPS OR RUC CYCLES DUE TO CONTINUING DEVELOPMENT.                   | 
!         SOME DETAILS:                                                       |
c         1. Removal of vestage Cray vector merge functions, reorganize
c            loop 30, move some repeated calculations to PARAMR,
c            other cleanup.
c         2. Cooper curve for ice nucleation instead of Fletcher curve.
c         3. Revision to "Bigg" freezing of water drops.
c         4. Remove inconsistencies between min mass and min diameter
c            of smallest particles for snow, ...
c         5. Change expression for collection efficiencies for collisions
c            involving cloud water (generally, decrease them).
c         6. Make formulations more consistent with Reisner et al 1998
c            (QJRMS).  
c         7. Fall velocities dependent on air density.
c         8. Autoconversion of rain via a Barry-type formulation is
c            an option instead of Kessler (via uncommenting).
!                                                                             |
!       19 JAN 00: RECEIVED WHAT IS CONSIDERED A STABLE VERSION BY NCAR FOLKS |
!                  FROM KEVIN MANNING.                                        | 
!
!       Late Oct and erly Nov 2000: further revisions resulted from 
!        collaboration with Greg Thompson and Roy Rasmussen, mainly for making
!        M-P zero intercept for rain variable to allow for smaller drop
!        sizes at low mixing ratios, and for predicting active ice
!        nuclei.       
!
!       22 Nov 03: Modifications by Greg Thompson to match MWR Part1 paper    |
!       Changes made by GT reflect MM5 v3.6.2 (see comment block below).      |
!
!       01 Dec 04: Modifications by Greg Thompson                             |
!       Changes made by GT reflect MM5 v3.7 (see comment block below).        |


      SUBROUTINE EXMOISG(J,IN,IST,IEN,dtrain,dt,kl)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                                                                     C
C     THIS SUBROUTINE COMPUTES THE MOISTURE TENDENCIES                C 
C     THE PROGNOSTIC EQUATIONS INCLUDE                                C 
C     WATER VAPOR (QVTEN), CLOUD WATER (QCTEN), RAIN WATER (QRTEN),   C
C     CLOUD ICE (QITEN), SNOW (QNITEN), GRAUPEL (QGTEN),              C
C     NUMBER CON OF CLOUD ICE (QNCTEN),                               C
C     IN = 1 : FOR LARGE DOMAIN                                       C
Cjmb     = 2 : FOR NESTED DOMAIN   <===  no nesting in MAPS version.  C
C                                                                     C
C     ALL THE OTHER ARGUMENTS ARE PASSED FROM SUBROUTINE "SOLVE3" AND C
C     EXPLAINED IN "SOLVE3".                                          C
C                                                                     C
C     comments added frm Greg T., 5 Aug 99                  C
C     THE SCHEME IS MODIFIED AGAIN FOR RELEASE-3-xx BY GREG THOMPSON, C
C     AND ROY RASMUSSEN INCLUDING INPUT FROM JOHN BROWN (NOAA/FSL)    C
C     MAJOR CHANGES INCLUDE                                           C
C       BUG FIXES TO PREVIOUS RELEASE                                 C
C       ICE NUMBER CONCENTRATION CORRECTED (PROGNOSED ONCE AGAIN)     C
C       AUTOCONVERSION CLW->RAIN CHANGED TO 0.25 G/KG  (FROM 0.1)     C
!jmb     Threshhold is set in PARAMR                                  | 
C       FREEZING OF CLOUD DROPS NOW INITIATES NCON CORRECTLY          C
C       ACCORDING TO MIN ICE MASS INSTEAD OF CNP NUMBER               C
C       PRI,PRD,PREI,PREG NOW ADJUSTED _BEFORE_ COMPUTING DEPAC+OTHER C
C       INIT OF CLOUD WATER NOW USES CONSISTENT RSLF FUNCTION         C
C       FEW VARIABLE NAMES CHANGED/CLARIFIED                          C
C    2 NOV 2000 MEETING WITH JOHN BROWN                               C
C       MULTIPLIED THE FOLLOWING CONSTANTS FOUND IN JRG COMMON BLOCK  C
C       BY RONV SINCE THEY CONTAIN RON IN NUMERATOR: FRD1,CIR,CIRF,   C
C       CSR,CRS,CRG,ACRCR,DEPR1                                       C
C       ADDED SUM PSSACW+PGSACW=PSACW LINE ABOVE PISPL SINCE EACH VAL C
C       COULD HAVE CHANGED IN CONSERVATION CHECK                      C
C       CHANGED AB TO ABW IN DENOMINATOR OF PMLTEV,PMLTGE SINCE IT IS C
C       WATER EVAPORATING OFF THE SNOW/GRAUPEL NOT ICE SUBLIMATING    C
C       CONSTRAINED DUM11 GREATER THAN ZERO IN FINAL DO LOOP          C
C    RELEASE 3.6:                                                     C
C    30 JUL 2002                                                      C
C       BUG FIXES FOR RON2 AND NCON UNITS; FUDGEF CHANGED 1.0 TO 0.5; C
C       MOVE DIACE_min, RHO_not AND RONV CONSTANTS TO PARAMR/JRG;     C
C       AUTOCONVERSION DONE EARLIER; COMMENT IIWARM VARS AND GOTOS;   C
C       COMMENT OUT PRACS/PSACR AND ALPSNOW/ALPRAIN, SET ALPHARS=0.0; C
C       SUBSTITUTE TEMPERATURE-DEPENDENT SONV (Y-INTERCEPT EXP        C
C       DISTRIB FOR SNOW) FOR PREV MASS-DEPENDENT SONV.               C
C    06 NOV 2002                                                      C
C       CHANGE SNOW-TO-GRAUPEL CONVERSION.  PREVIOUSLY, BASED ON      C
C       MURAKAMI, RIMING GROWTH (PSACW) NEEDED TO EXCEED DEPOSITIONAL C
C       (PREI) GROWTH.  NOW PSACW MUST BE 2.5 TIMES GREATER THAN PREI.C
C       ALSO SWAPPED KESSLER AUTOCONVERSION FOR BERRY AND REINHARDT   C
C       SCHEME IMPLEMENTED AS IN WALKO ET AL. (1995).                 C
C    02 DEC 2002                                                      C
C       INCLUDE SCHMIDT NUMBER TO ONE-THIRD POWER (0.84) IN PREI,     C
C       PRE, PMLTEV (BUG FIX BY DR. GERHARD KRAMM)                    C
C    15 MAY 2003                                                      C
C       PSACW MUST BE 2.5 TIMES GREATER THAN PREI BEFORE CREATING     C
C       GRAUPEL.  MAJOR BUG FIX TO BERRY/REINHARDT AUTOCONV AFTER     C
C       DISCOVERING MISTAKES IN JOURNAL PAPERS.  REQUIRE MIN DIAMETER C
C       OF ICE GREATER THAN 100 MICRONS BEFORE RIMING ICE BASED ON    C
C       PRUPPACHER AND KLETT, 1997 (PAGE 600).  ICE MULTIPLICATION    C
C       SECTION: CHANGED PSACW TO PGACW PER DISC WITH I. GERESDI.     C
C    01 SEP 2003                                                      C
C       PSACW MUST BE 3.0 TIMES GREATER THAN PREI FOR GRAUPEL INIT.   C
C       CHANGED AFTER MORE TESTING AGAINST GERESDI BIN MODEL AND TO   C
C       MATCH THE THOMPSON, RASMUSSEN, MANNING MWR-PART 1 PAPER.      C
C    MM5 RELEASE 3.7:                                                 C
C    06 DEC 2004                                                      C
C       Mods to the rain y-intercept value, RONV, (parameters in      C
C       paramr.F) to eliminate "rain-gush" effect.                    C
C       Added parameter xnu calculated from CNP to make maritime CCN  C
C       use broad drop spectra but narrow the distrib (decrease       C
C       dispersion) as CNP increases to continental values.  Net      C
C       effect: increase autoconversion for maritime CNP values;      C
C       little change for continental.                                C
C       Graupel intercept parameter, GONV, was not correct for gamma  C
C       size distrib as described in MWR Part1.  Went back to old exp C
C       distrib and not allow GONV to exceed old M-P value of 4.E6.   C
C       Assigned min fallspeed of cloud ice to 0.3 m/s.               C
C       Changed aggregation of cloud ice according to new fallspeed.  C
C       A few other minor bug fixes.                                  C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

cjmb--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cjmb    DT in EXMOISG is time interval between calls to EXMOISG
cjmb--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cjmb--Add metcon here to get physical constants, rather than
cjmb   setting psbly different values here.--23jan96 

      include 'IMPLICIT'
      include 'METCON'
      include 'PARAME'
      include 'PARAMX'
      include 'MOLTS'
      include 'LOCATION'

      include 'PARAM3'

cjmb--Put in pmoist moisture vrbls that are not basic physical constants.
cjmb   Define the latter in metcon.--20feb96
      include 'PMOIST'
      include 'POINT3D'
      include 'POINT2D'

      include 'PMICRPH'
      include 'NAVYPBL'
      include 'CUGREL'
csms$distribute(grid_dh, <mix>, <mjx>) begin
cjmb--Vrbls in /JRG/ are defined in PARAMR.  /JRG/ is in 'PMICRPH'  

      INTEGER IT,NSTEP

      INTEGER J,IN,IM,IST,IEN,K,KL,KK,I,N
      REAL PD,EVS,EIS,QVS1,QVI1,FALTNDN
c     REAL RNCONT,sonv1,GONV,T0,TF,TEMPFL,DT,DTMI,XNC,VT2R,vt2i
c    1,VT2S,
c    1VT2G,VT2nr,VT2NS,A1,A2,DIACE,DCLO,UDICE,UDCLO,RPHI,EIC1,PIACW1,
c    1PRDTEMP,CLIC,DMGO,DMGI,TAU2,AGIC,DEPAC,PRCITO,XLATF,RATIO,
c    1ALPHARS,D1,D2,D3,TEMP,AGIS,RHO2,SLOR1,RGV1,RGVM,SLOS1,SLOG1,
c    1FALTNDR,faltndi,FALTNDS,FALTNDG,faltndnc

      REAL t0,xmi,tf,dt,supice,xnc,fudgef,sum_dep,
crho_not,slor_r1,
c    1     slos_r1,const_ns1,const_ns2,slog_r1,const_ng1,
c    2     const_ng2,GAMMA3,XM0G,XM0S,XR0S,FALTNDN,
     1  drno,stoke_r,
     3     eff_cr,dsno,stoke_s,eff_cs,dgro,stoke_g,eff_cg,
     1dtau,alpsnow,alprain,alphars,thetah,d1,
     1A1,A2,DIACE,DCLO,UDCLO,RPHI,EIC1
     1,PIACW1,PRDTEMP,PSAGI,PISPL_FS,PISPL_FG
     1,CLIC,DMGI,DEPAC,C1_AG,TEMP_ICE,TEMP_NC,RLU
     1,XLATF,RATIO,R2,R3
     1,TEMP,RHO3,SLOR1,RGVM,SLOS1,SLOSN,SLOG1,SLOGN
     1,UPDT_R,UPDT_S,UPDT_G,UPDT_I,UPDT_N,DIAMI
     1,FALTNDR,faltndi,FALTNDS,FALTNDG
     1,BandR_L2,BandR_T2
     1,RSIF, RSLF

cjmb--The vrbl PI in /JRG/ is 3.14....., set in paramr.--31jan96

       real exitten,exiqvten,exiqcten,exiqrten,
     1      exiqniten,exiqncten,exiqiten,exiqgten
       common/exmoisten/ exitten(mix,mjx,mkx),
     .       exiqvten(mix,mjx,mkx),
     .       exiqcten(mix,mjx,mkx),exiqrten(mix,mjx,mkx),
     .       exiqiten(mix,mjx,mkx),exiqniten(mix,mjx,mkx),
     .       exiqgten(mix,mjx,mkx),exiqncten(mix,mjx,mkx)

cjmb--Declare utility and scratch constants and variables 
      real opsb,parma,qv,rhoqs,rhoqg,rho3g,shb
      real temp_c

C ARRAYS FOR BASE VARIABLES

      REAL TAOUT(MIX,MKX),PRES(MIX,MKX),RHOd(MIX,MKX),
     1     TTEN(MIX,MKX),SCR7(MIX,MKX),SCR6(MIX,MKX)
     1     ,TTENPRE(MIX,MKX),RHOe(MIX,MKX)
c     real exitten

C ARRAYS FOR Qv

      REAL QAOUT(MIX,MKX),QVQVS(MIX,MKX),QVQVSI(MIX,MKX),
     1     QQVS(MIX,MKX),QVSI(MIX,MKX)
     1    ,AB(MIX,MKX),ABI(MIX,MKX),ABW(MIX,MKX)
     1    ,PREG(MIX,MKX),QVTEN(MIX,MKX),QVTENPRE(MIX,MKX)
c     real exiqvten

C ARRAYS FOR Qc

      REAL CLOUD(MIX,MKX),PRC(MIX,MKX),
     1     PSACW(MIX,MKX),PGACW(MIX,MKX),
     1     PRA(MIX,MKX),QCTEN(MIX,MKX),SCR3(MIX,MKX)
     1    ,QCTENPRE(MIX,MKX)
c      real exiqcten

C ARRAYS FOR Qr

      REAL RAIN(MIX,MKX),SLOR(MIX,MKX),PIACR(MIX,MKX),
     1     PGACR(MIX,MKX),PRE(MIX,MKX),
     1     QRTEN(MIX,MKX),SCR4R(MIX,MKX),FR(MKX),
     1     QRTENPRE(MIX,MKX),
     1     FALOUTR(MKX) ,RONV(MIX,MKX)
c      real exiqrten,pcpnrb
      real vt2r
      real pcpnrb
   

cjmb--Note that the array pre is rain evaporation xcpt at end of exmoisg
cjmb   (sat adjustment prn) where it is pressure.  I have changed this in
cjmb   MAPS version so that pres is used in saturation adjustment instead of
cjmb   pre for pressure.

C ARRAYS FOR Qi

cjmb  REAL ICE(MIX,MKX),PRI(MIX,MKX),PRCI(MIX,MKX),
      REAL ICE(MIX,MKX),PRI(MIX,MKX),PSCNI(MIX,MKX),
     1     PRAI(MIX,MKX),PRACI(MIX,MKX),PISPL(MIX,MKX)
     1    ,PRD(MIX,MKX),QITEN(MIX,MKX),scr4i(mix,mkx)
     1    ,PIACW(MIX,MKX),PIFCW(MIX,MKX)
     1    ,QITENPRE(MIX,MKX),fi(mkx),falouti(mkx)
c      real exiqiten,pcpnib
      real vt2i
      real pcpnib

C ARRAYS FOR Qs

      REAL SNOW(MIX,MKX),SLOS(MIX,MKX),PSACR(MIX,MKX),
     1     PGSACW(MIX,MKX),PREI(MIX,MKX),
     1     PSMLT(MIX,MKX),PMLTEV(MIX,MKX),QNITEN(MIX,MKX),
     1     SCR4S(MIX,MKX),FS(MKX),FALOUTS(MKX),
     1     PRACS(MIX,MKX),PSSACW(MIX,MKX),
     1     SONV(MIX,MKX)
     1    ,QNITENPRE(MIX,MKX)
c      real exiqniten,pcpnsb
      real vt2s
      real pcpnsb

C ARRAYS FOR Qg

      REAL GRAUPEL(MIX,MKX),gonv(mix,mkx),
     1     PGMLT(MIX,MKX),PMLTGE(MIX,MKX),
     1     PGACRM(MIX,MKX),PGACWM(MIX,MKX),
     1     QGTEN(MIX,MKX),SCR4G(mix,MKX),FG(MKX),
     1     FALOUTG(MKX),SLOG(MIX,MKX),PGFR(MIX,MKX),
     1     PICNG(MIX,MKX),
     1     PGIACW(MIX,MKX),PGEMB(MIX,MKX),
     1     QGTENPRE(MIX,MKX)
c      real exiqgten,pcpngb
      real vt2g
      real pcpngb

C ARRAYS FOR NCI

      REAL NCON(MIX,MKX),NI_AG(MIX,MKX),NI_RS(MIX,MKX)
     1     ,NI_CG(MIX,MKX),NI_DE(MIX,MKX),NI_EV(MIX,MKX)
     1     ,QNCTEN(MIX,MKX),QNCTENPRE(MIX,MKX),fnc(mkx)
      real faloutn(mkx)
      REAL scr4n(mix,mkx)
c      real exiqncten

C EXTRA ARRAYS

      REAL DUM11(MIX,MKX),DUM21(MIX,MKX),DUM31(MIX,MKX),
     1     SCR4(mix,MKX),SCR8(mix,MKX),SCR9(mix,MKX)
     1     ,RHO_FAC(mix,MKX)
      real dtrain
      real dsigma(mkx)
C
      integer itest,jtest, istat
      logical iiwarm
csms$distribute end

! END OF DECLARATIONS
*******************************************************************************
 
convert metcon vrbl names to conventional mm5 usage.  Some also done in 
cjmb   inithybv and put into pmoist. 
      cp   = cpd_p
      g    = g0_p
      r    = rd_p
      rv   = rv_p
      xlv1 = dlvdt_p

cjmb--Apparently, MM5 usage is to use TO for 273.15.  TO is declared in PMOIST
c      and set in INITHYBV when IEXICE = 1 (IEXICE set in HYBCSTDV)--9sep96
      tO   = t_std_p
      iiwarm = .false.   !  Warm rain only switch
      if (iiwarm) tO=173.0
!jmb--Just to make sure we are covered (t0 is local), 
      t0=tO
!     RHO_NOT calculated in PARAMR and passed thru PMICRPH
!     rho_not = 101325.0/(r*298.15)  !  Standard density for fall velocities

!jmb-- Default small values for mixing ratios (protect against underflows)
!       now set in PARAMR.--22jan99
      

c      jtest = 15
c      itest = 100

*******************************************************************************

cjmb--Loop 15 sets certain MM5-format variables (upside down) on i-k slabs
c      needed in guts
c      of calculations.  This loop 15 is tailored specifically to MAPS/RUC. 

      DO 15 K=1,kl

        kk=kl-k+1
cjmb--  Invert k indices in this loop to account for MAPS
c        indexing starting frm 1 at lowest model level and increasing
c        with height.--16jan96
cjmb--  Loop 15:  kk is maps, k for MM5 formats. (changed 16dec00)

        do 15 i = ist,ien

!       MM5 version here zeroes out liquid and solid mixing-ratio tendencies 
!        if they are less than R1.--jmb 26jan00
 
cjmb      qaout(i,k)= max(r1,qvb(i,j,kk))
          qaout(i,k)= max(r1,qva(i,j,kk))
cjmb--    Devirtualize to get temp that routine needs--14may96
          qv = qaout(i,k)  
          shb = qv/(1.+qv)
          parma = 1./(1.+.6078*shb)
cjmb      taout(i,k)= tb(i,j,kk)*(exn(i,j,kk)/cp)*parma
          taout(i,k)= ta(i,j,kk)*(exn(i,j,kk)/cp)*parma
c         IF(TAOUT(I,k).LE.0.) PRINT*,'TEMP LESS THAN ZERO',
c    1     i,kk,taout(i,k),' crash will soon occur'
c         IF(TAOUT(I,k).LE.0.) return 

!jmb      Convert maps pressure to pascals--
!          so that densities of hydrometeors in loop 30 
!          are correctly computed
!          (i.e., using partial pres of dry air(=pd)--not a big deal).--14may96
!          Note that dry-air density (RHOD) times mixing ratio of constituent x
!          gives density of constituent x.--9feb96
          pres(i,k) = p(i,j,kk)*1.E5
cjmb      pd = .62198*pres(i,k)/(qvb(i,j,kk) + .62198) 
          pd = .62198*pres(i,k)/(qva(i,j,kk) + .62198)
cdec98--  RHOD -- Density of dry air (density of hydrometeors in rate eqns)
c         RHOE -- Density of moist air (fallout, evaporation, sublimation)
!jmb      I have tried to preserve precision in use of rhod vs rho.
!          As noted above, where density of hydrometeors aprs in rate eqns
!          (density * mixing ratio)
!          rhod is appropriate.  Where density times a mixing-ratio change
!          rate aprs in number concentration change rates, I have multiplied
!          by rhod because of the definition of mixing ratio.  Where
!          a mixing ratio change rate contains division by density to the
!          power unity, I use rhod; in this case the division is to make 
!          the units of the production term appropriate to mixing ratio change.
!          Where diffusion or fallout is
!          involved, I use rho, the latter for obvious reasons, the former
!          because I think the diffusion depends on the total density,
!          not just on the dry-air density (I cant prove this).--10feb00
!           
          RHOd(I,k) = pd/(R*TAOUT(I,k))
          rhoe(i,k) = pres(i,k)/(r*taout(i,k)*(1.+.6078*shb))
          rho_fac(i,k) = (rho_not/rhoe(i,k))**0.5 

C SATURATION VAPOR PRESSURE FOR T > TO (273 K)
cjmb--    EVS,EIS in cb (original NCAR EXMOISG code).  Convert to 
!          pascals by factor of *1000 below. 

Cgt       EVS=SVP1*EXP(SVP2*(TAOUT(I,k)-SVPT0)/(TAOUT(I,k)-SVP3))       

C SATURATION VAPOR PRESSURE FOR T < TO (273 K)
Cgt       EIS=.611*EXP(22.514-6.15E3/TAOUT(I,k))

C     SATURATION MIXING RATIOS FOR WATER AND ICE

cjmb--    Here, EVS, EIS in cb, PRES in pa.
cjmb--    Used QQVS instead of QVS because QVS used as a global variable in
c          other parts of code.  
cjmb****Make sure QVS is not used in MAPS version.--17jan00

Cgt       qqvs(i,k) = .62198*evs*1000./(pres(i,k) - 1000.*evs)

cjmb--for ice saturation--11apr96

Cgt       qvsi(i,k) = .62198*eis*1000./(pres(i,k) - 1000.*eis)

C SATURATION MIXING RATIOS OVER WATER AND ICE (FLATAU&WALKO)      ! GREG T.
          qqvs(i,k) = RSLF(PRES(I,K),TAOUT(I,K))                  ! GREG T.
          qvsi(i,k) = RSIF(PRES(I,K),TAOUT(I,K))                  ! GREG T.

cdec98--  Need to ensure that there is no psblty of ice saturation vapor pres
cdec98     being larger than water in these formulae to avoid division by
cdec98     zero in loop 30.

          QVQVS(I,k)=QAOUT(I,k)/qqvs(I,k)
          QVQVSI(I,k)=QAOUT(I,k)/QVSI(i,k)

C DIFFUSIVITY OF WATER VAPOR  (Pruppacher & Klett, 1997 Eq 13-3)
          DUM11(I,K)=2.11E-5*((TAOUT(I,K)/TO)**1.94) *
     1     (101325./PRES(I,K))      

C DYNAMIC VISCOSITY OF AIR  (Pruppacher and Klett, 1997, Eq. 10-141)
          temp_c = taout(i,k)-TO
          if (temp_c .ge. 0.0) then
            DUM21(I,K)=(1.718 + 0.0049*temp_c) * 1.0E-5
          else
            DUM21(I,K)=(1.718 + 0.0049*temp_c -
     1                 1.2E-5*temp_c*temp_c) * 1.0E-5
          endif

c     V8 Lv FORMULA
!jmb--This frm Bolton (1980, MWxR), with TAOUT in deg K. --11may96
          DUM31(I,K)=3.1484e6-XLV1*TAOUT(I,K)

C THERMAL CONDUCTIVITY OF AIR  (Pruppacher & Klett: Eq. 13-18a)
          scr4(i,k)=(5.69 + 0.0168*temp_c) * 1.0E-5 *418.936

!jmb--    These a+b formulae can be found in Section 5.3.1.1 of Grell, Dudhia
!          and Stauffer (1994,1995;MM5 documentation).  Note that in the 
!          expression for A below there is a small approximation made that
!          the quantity (L_v/(R_v*T) >> 1.  Earlier versions of this A+B formula
!          may be found in RH83 and in D89.   

cjmb       ABI should be used for deposition/sublimation of graupel and snow
c          ABW should be used for evaporation of (condensation on)rain,
c          melting snow and melting graupel.  AB should not be used.--30oct00
C A + B IN FORMULA (B7) OF DUDHIA (1992) FOR T > TO
          AB(I,K)=RHOe(I,K)*XLS*XLS/(SCR4(I,K)*RV *
     1      TAOUT(I,K)*TAOUT(I,K))+1./(QQVS(I,K)*DUM11(I,K))

C A + B FOR T < TO (for A.36 in Reisner et al 1998)
          ABI(I,K)=RHOe(I,K)*xls*xls/(SCR4(I,K)*RV*
     1      TAOUT(I,K)*TAOUT(I,K)) + 1./(QVSI(I,K)*DUM11(I,K))

          ABW(I,K)=RHOe(I,K)*dum31(I,K)*dum31(I,K)/(SCR4(I,K)*RV*
     1      TAOUT(I,K)*TAOUT(I,K)) + 1./(QQVS(I,K)*DUM11(I,K))


C Latent Heat effects (reduces depositional growth of snow/graupel)
c           (Cotton&Anthes 1989 4-37)
cjmb--      I have not verified this formula.
            DUM11(I,K) = RHOe(I,K)*XLS*3.34E5/(SCR4(I,K)*RV*
     1       TAOUT(I,K)*TAOUT(I,K)*ABI(I,K))
            DUM11(I,K) = AMIN1(1.0, AMAX1(DUM11(I,K),0.0))

   15 CONTINUE

cjmb--Loop 17 is specific to MAPS/RUC--added 19jan99
     
      DO I=ist,ien
        precing(i,j)=0.
        precins(i,j)=0.
        precini(i,j)=0.
        precinr(i,j)=0.
cjmb--  Initialize grid-scale pcpn array for this grid row--18apr97
        grdpcpn(i,j) = 0.
      enddo
        do 17 k = 1,kl
        kk=kl-k+1
           do 17 i=ist,ien
               qcten(i,kk)=0.
               qiten(i,kk)=0.
               qncten(i,kk)=0.
!jmb--         In the following IF block, using a colder temp might enhance
!               the psblties of explicit pcpn through latent heat release
!               of detrained supercooled liquid water.  In any case, the
!               temperature would logically be assigned that used in the
!               convective scheme for freezing of condensate in updrafts. 
C  -- modified - Stan B - 12 July - based on John and Tanya's experiment
C         -- changed from 268 (-5C) to 253 (-20 C)
               if(taout(i,kk).gt.253.)then
                 qcten(i,kk)=outtqc(i,j,kk)*psa(i,j)
               elseif(taout(i,kk).le.253.)then
                 qiten(i,kk)=outtqc(i,j,kk)*psa(i,j)
Cjmb--3jan05     qncten(i,kk)=(outtqc(i,j,kk)*rhod(i,kk)/xm01)*psa(i,j)        
                 qncten(i,kk)=(outtqc(i,j,kk)/xm01)*psa(i,j) ! jb--#/kg/s       
               endif
               qrten(i,kk)=0.
               qniten(i,kk)=0.
               qgten(i,kk)=0.
               tten(i,kk) = psa(i,j)*(ttenv(i,j,kk)+outtem(i,j,kk))
               qvten(i,kk)= psa(i,j)*(qvtenv(i,j,kk)+outtq(i,j,kk))
               ttenpre(i,kk)   = tten(i,kk)
               qvtenpre(i,kk)  = qvten(i,kk)
c jmb--Initialize maps tendencies for this i row.--28may96
               exitten(i,j,k)  = 0.
               exiqvten(i,j,k) = 0.
               exiqcten(i,j,k)  = 0.
               exiqiten(i,j,k)  = 0.
               exiqrten(i,j,k)  = 0.
               exiqniten(i,j,k) = 0.
               exiqgten(i,j,k)  = 0.
               exiqncten(i,j,k) = 0.
c jmb--Initialize grid-scale pcpn array for this grid row--18apr97
  17  CONTINUE



C---BEGIN PRODUCTION TERMS CALCULATION:
cjmb--The following comment pertains to MM5.  
C    ALL THE PRODUCTION TERMS ARE BASED ON T-1 (I.E. XXB) VARIABLES
cjmb--As of Sep 97, for MAPS/RUC application, all production terms are
c      calculated using predicted
c      liquid and solid mixing ratios at time step ktau+1,
c      which are updated at this point to
c      include effects of advection KTAU to KTAU+1.
c      This change was made to save critically valuable memory at NCEP.
c      Theta_v and q_v were still using final values at KTAU.
cjmb--Try using "A" values (that is, KTAU+1) out of HYBCST for theta_v
c      and q_v for consistency.--31Aug99
cjmb--****Think about this chg a ltl more--What are current "A" values of 
c      vrbls as brought in frm HYBCST????****

      DO 20 K=1,KL
        kk = kl-k+1
cjmb--  In loop 20, k is MM5 (upside down) and kk is MAPS--6jun96
        DO 20 I=ist,ien

cjmb--  In MM5 it is necessary to divide the mixing ratios and number
c        concentrations by psb=psfc-ptop
c        since it is psb*mixing ratio or number concentration that is 
c        predicted by MM5--
c        For maps vrbls we can do this directly w/o dividing by psb--6jun96
          cloud(i,k)  =amax1(r1,qca(i,j,kk))
          ice(i,k)    =amax1(r1,qia(i,j,kk))
          rain(i,k)   =amax1(r1,qra(i,j,kk))
          snow(i,k)   =amax1(r1,qnia(i,j,kk))
          graupel(i,k)=amax1(r1,qga(i,j,kk))
Cgt       ncon(i,k)   =amax1(r1,qnca(i,j,kk))
          ncon(i,k)   =amax1(r1,qnca(i,j,kk)*rhod(i,kk))
!jmb      QNCA is number concentration per unit mass
!jmb--    NCON is number concentration per unit volume, i.e., per m**3 

!jmb--    Note that SLOR, SLOS and SLOG are inverse slopes!!!! Also,
!          RONV,SONV,GONV, M-P zero intercept values, normalized by
!          max allowable values.  SLOx_R1 are default inverse slopes 
!          precalculated in PARAMR to save computations at grid points
!          where q_x set to default small values above.
!   --rain
!jmb      ronv(i,k) = ron
          ronv(i,k) = 1.
          slor(i,k) = slor_r1
          IF (RAIN(I,K) .GT. R1) THEN
            ronv(i,k) = (const1r*tanh((qr0 - rain(i,k))/delqr0) +
     1       const2r)/ron       
            SLOR(I,K)=(RHOd(I,K)*RAIN(I,K)/(TOPR*RONV(I,K)))**0.25
          ENDIF
!jmb      RONV(I,K) = RONV(I,K)/RON

!   --snow
!jmb      sonv(i,k)=son
          sonv(i,k)=1.
          slos(i,k) = slos_r1
          rhoqs=RHOd(i,k)*snow(i,k)
          IF (SNOW(I,K) .GT. R1) THEN
!jmb--      This formula gives identical results for SONV as the formula used
!            in 4dec98 NCAR/RAP version.
Cgt         sonv(i,k)=const_ns1*(rhoqs**const_ns2) 
!           sonv(i,k) = min(son,sonv(i,k))
Cgt         sonv(i,k) = min(1.,sonv(i,k)/son)
C.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979    ! GREG T.
           temp_C = min(-0.001, TAOUT(I,K)-273.15)                       ! GREG T.
           sonv(i,k) = (min(2.0E8, 2.0E6*exp(-0.12*temp_C)))/son         ! GREG T.
            slos(i,k)=(rhoqs/(tops*sonv(i,k)))**0.25
          ENDIF
!jmb      sonv(i,k)=sonv(i,k)/son

!   --graupel
!jmb--    This based on Roy Rasmussens procedure (feb96) for including
!          variable M-P intercept value for graupel--12jun96

          rhoqg=RHOd(i,k)*graupel(i,k)
!jmb      gonv(i,k)=gon
          gonv(i,k)=1.
          slog(i,k)=slog_r1
          IF (GRAUPEL(I,K).GT.R1) THEN
            gonv(i,k)=const_ng1*(rhoqg**const_ng2)
!jmb--      Note: this formula (with the extra constants defined in PARAMR) 
!            gives very similar results to the 28jul98 version
!            of EXMOISG frm ncar/rap.--13aug98

            gonv(i,k) = max(1.e4, min(gonv(i,k),gon))  
!jmb--      Where did the 1.e4 come frm?
!            This thought necessary by Roy R.  Jul98 
            gonv(i,k)=gonv(i,k)/gon
            slog(i,k)=(rhoqg/(topg*gonv(i,k)))**0.25
          ENDIF
!jmb      gonv(i,k)=gonv(i,k)/gon
!jmb-------------end of procedure for diagnosing slopes, intercepts for 
!                  hydrometeors.   

!     ENDDO
 20   CONTINUE


C PROCESSES TO BE DOMAIN AVERAGED AND WRITTEN OUT AT EVERY TIME STEP

      DO 22 K=1,KL 
        DO 22 I=ist,ien
          PRI(I,K)=      0.
          PIFCW(I,K)   = 0. 
          PGFR(I,K)    = 0.
          PSCNI(I,K)   = 0.
          PRAI(I,K)    = 0.
          PRACI(I,K)   = 0.
          PIACR(I,K)   = 0.
          PSACR(I,K)   = 0.
          PRACS(I,K)   = 0.
          PSACW(I,K)   = 0.
          PGSACW(i,k)  = 0.
          PGACW(I,K)   = 0.
          PGACR(I,K)   = 0.
          PREG(I,K)    = 0.
          PRD(I,K)     = 0.
          PREI(I,K)    = 0.
          PRC(I,K)     = 0.
          PRA(I,K)     = 0.
          PRE(I,K)     = 0.
          PSMLT(I,K)   = 0.
          PMLTEV(I,K)  = 0.
          PGMLT(I,K)   = 0.
          PMLTGE(I,K)  = 0.
          PGACRM(I,K)  = 0.
          PGACWM(I,K)  = 0.
          PIACW(I,K)   = 0.
          PICNG(I,K)   = 0.
          PGIACW(I,K)  = 0.
          NI_AG(I,K)   = 0.
          NI_RS(I,K)   = 0.
          NI_CG(I,K)   = 0.
          NI_DE(I,K)   = 0.
          NI_EV(I,K)   = 0.
          PISPL(I,K)   = 0.
          PSSACW(I,K)  = 0.
          PGEMB(I,K)   = 0.
!       ENDDO
!     ENDDO 
   22 CONTINUE


C FORMULAS COME FROM DUDHIA (1989, JAS, 46), 
C RUTLEDGE AND HOBBS (1983, JAS, 40),                    {RH1},
C RUTLEDGE AND HOBBS (1984, JAS, 41),                    {RH},
C MURAKAMI (1989, JMSJ, 68),
C LIN ET AL. (1983, JCAP, 22)
C HSIE ET AL. [1980, JAM (?), 19]
C KOENIG (1972, MWR, 100)
C Meteorological Research Institute of Japan (#28, 1991) {MRI}
C AND REISNER ET AL. (1998, QJRMS 124, 1071-1107)        {R}

cjmb--Need to check if RHO and RHOD are used appropriately in loop 30 (????)
      DO 30 K=1,KL

        DO 30 I=ist,ien

c         think about doing a do loop for t<to

Cgt       XMI=0.
          XMI=ICE(I,K)*RHOd(I,K)/NCON(I,K)                              ! GREG T.
          VT2R=(FRAIN*SLOR(I,K)**BR)*rho_fac(i,k)
          VT2R=AMIN1(VT2R, 9.0)                                         ! GREG T.
          VT2S=(FSNOW*SLOS(I,K)**BS)*rho_fac(i,k)
          VT2S=AMIN1(VT2S, 2.0)                                         ! GREG T.
          VT2G=(FGRAUPEL*SLOG(I,K)**BG)*rho_fac(i,k)
          VT2G=AMIN1(VT2G, 10.0)                                        ! GREG T.
          DIACE=(6.*RHOd(I,K)*ICE(I,K)/(PI*DICE*NCON(I,K)))**0.3333
          DIACE=AMAX1(AMIN1(2.*XR0S, DIACE), DIACE_min)                 ! GREG T.
Cgt       IF (DIACE .GT. 2.*XR0S) DIACE = 2.*XR0S
          VT2I=700.*DIACE*RHO_FAC(i,k)                                  ! GREG T.
          VT2I=AMAX1(AMIN1(1.0, VT2I), 0.3) ! V 3.7 29 Dec 04             GREG T.
Cgt       DIACE=AMAX1(DIACE_min, ((6.*RHOd(I,K)*ICE(I,K)                ! GREG T.
Cgt  +            /(PI*DICE*NCON(I,K)))**0.3333) )                      ! GREG T.
Cgt       VT2I=700. * AMIN1(DIACE, 2.*XR0S) * RHO_FAC(i,k)              ! GREG T.

C *** test - put in 1 m/s ice fallout speed to
C        see if it helps get rid of nagging cirrus cover
C        - Stan B. with advice from John B.
Cjmb *** With v3.7 of EXMOISG, this 1 m/s fixed fallout was replaced 

Cgt       vt2i = 1.


          A1 = 0.
          A2 = 0.
          xnc = 0. ! Suggested by Bill Hall--3Jan01
          IT=NINT(TAOUT(I,K) - 273.16)
          dclo = (6.*rhod(i,k)*cloud(i,k)/(PI*DRAIN*CNP))**0.3333
          dclo = AMAX1(1.e-6, dclo)                                     ! GREG T.

          IF (CLOUD(I,K) .GT. R1) THEN  ! Not in 19jan00 ncar version 

C---AUTOCONVERSION OF CLOUD WATER TO RAINWATER R: (A.60)
cjmb--      The autoconversion threshhold, QCTH, is set in PARAMR.  This
cjmb         threshhold should be set to at least R1 to prevent very small
cjmb         amounts of rainwater from occurring when CLOUD is 
!            essentially zero. 
cjmb--       This term should also be set to zero whenever t < -40C.--19mar97

Cgt         PRC(I,K)=AMAX1(0.,QCK1*(CLOUD(I,K)-QCTH))

C+---+-----------------------------------------------------------------+! GREG T.
CC      ! Berry and Reinhardt autoconversion (1974, Part II) as in      ! GREG T.
CC      ! Walko et al., (1995) but WITH TYPOS CORRECTED IN BOTH!!!      ! GREG T.
Cgt  Autoconversion is strongly dependent on assumed distrib shape and
Cgt  dispersion parameter, xnu.  For widescale use, CNP=100 produces
Cgt  xnu=0;  for known case studies, set CNP properly and utilize the
Cgt  xnu code that is commented below.  Xnu=0 produces the highest
Cgt  autoconversion possible - mostly characteristic of maritime
Cgt  environments and closer to the old Kessler scheme above.
Cgt
Cgt         BandR_L2 = 0.027*cloud(I,K)*(6.25E18*(DCLO**4)*(1.+xnu)**-0.5! GREG T.
Cgt  +                       - 0.4)                                     ! GREG T.
Cgt         BandR_T2  = 3.7/(rho(I,K)*cloud(I,K)) / (0.5E6*DCLO         ! GREG T.
Cgt  +                       *(1.+xnu)**(-1./6.) - 7.5)                 ! GREG T.
            BandR_L2 = 0.027*cloud(I,K)*(6.25E18*(DCLO**4) - 0.4)
Cjmb        BandR_T2  = 3.7/(rho(I,K)*cloud(I,K)) / (0.5E6*DCLO - 7.5)  ! GREG T.
            BandR_T2  = 3.7/(rhod(I,K)*cloud(I,K)) / (0.5E6*DCLO - 7.5) ! GREG T.
            if (BandR_L2.gt.0.0 .and. BandR_T2.gt.0.0)                  ! GREG T.
     +      PRC(I,K) = BandR_L2/BandR_T2                                ! GREG T.
C+---+-----------------------------------------------------------------+! GREG T.

            IF(RAIN(I,K).GT.R1)THEN
C---ACCRETION OF CLOUD WATER BY RAINWATER R: (A.61)
              DRNO = 4.*SLOR(I,K)
              STOKE_R = DCLO*DCLO*VT2R*DRAIN/(9.*DUM21(I,K)*DRNO)
              EFF_CR = STOKE_R*STOKE_R/((STOKE_R+0.5)*(STOKE_R+0.5))
              EFF_CR = MAX(0.0, MIN(1.0, EFF_CR))
              PRA(I,K)=ACRCR*RONV(I,K)*EFF_CR/EFCR*SLOR(I,K)**BACRCR
     1         *CLOUD(I,K)
            ENDIF
          ENDIF ! cloud > r1

          IF(RAIN(I,K).GT.R1)THEN
C...EVAPORATION OF RAINWATER; R: (A.62)
            PRE(I,K)=DEPR1*RONV(I,K)*(QAOUT(I,K)/QQVS(I,K)-1.)*(
     1       0.78*SLOR(I,K)*SLOR(I,K)+((DEPR2*RHOe(I,K)
     2       /DUM21(I,K))**0.5)*DEPR3*0.84*SLOR(I,K)**(DEPR4))
     3       /ABW(I,K)
            pre(i,k) = min(0.,pre(i,k)) ! Bill Hall 3jan01
cc          Eliminate condensational growth of rain--shud go to cld instead 
          ENDIF 
          IF(IIWARM)GOTO 702

          SUPICE=(QAOUT(I,K)-QVSI(I,K))/DT 
cjmb--    SUPICE > 0 for supersaturation wrt ice,
cjmb             < 0 for subsaturation.

C INITIATION OF CLOUD ICE  (R-A.21) [Either Cooper or Fletcher
c          depending on constants set in PARAMR]
C         COOPER CURVE is frm Fig. 1 - WISP Scientific Overview Document
cc        IF( ((QVQVSI(I,K).GT.1.12) .AND. (TAOUT(I,K).LT.273.15)) 
          IF( ((QVQVSI(I,K).GT.1.05) .AND. (TAOUT(I,K).LT.273.15)) ! 27oct00 
     .                                .OR.       
     .     ((QVQVS(I,K).GT.1.0) .AND. (TAOUT(I,K).LT.268.15)) )
     .     THEN
cc          XNC=TNO*EXP(ATO*(TO-AMAX1(TAOUT(I,K),246.)))
            XNC=TNO*EXP(ATO*(TO-AMAX1(TAOUT(I,K),240.)))                 ! GREG T.
            PRI(I,K)=AMAX1(0.,XM01/RHOd(I,K)*(XNC-NCON(I,K))/DT)
          ENDIF

C INITIATION OF CLOUD ICE VIA MEYERS EQN 2.4
c         IF ( (QVQVSI(I,K).GT.1.0.AND.TAOUT(I,K).LE.268.0) .OR
c     +           . (QVQVS(I,K).GT.1.0.AND.TAOUT(I,K).LE.271.0) ) THEN
c               XNC = (EXP(-0.639 + 12.96*(QVQVSI(I,K)-1.0)))*1000.0
c               PRI(I,K)=AMAX1(0.,XM01/RHOe(I,K)*(XNC-NCON(I,K))/DT)

C FREEZING OF CLOUD DROPLETS, MRI (11-19), R: (A.22)

cjmb--    PIFCW (Formerly NUFCI) .ge. 0. for temps blo -5C 
c          (This temp has jumped around quite a bit 
c          over the past year.--30aug99) 
cjmb--     CNP is 1.E8 m^-3 in R (page 1096).

          IF(CLOUD(I,K).GT.R1.AND.TAOUT(I,K).LT.268.15)THEN
            PIFCW(I,K)=BP*(EXP(AP*(TO-TAOUT(I,K)))-1.)*RHOd(I,K)*
     1       CLOUD(I,K)**2/(CNP*DRAIN)
          ENDIF
            
          IF(RAIN(I,K).GT.R1)THEN
C FREEZING OF RAIN, LIN ET AL. (45), R: (A.56)
cjmb--      PGFR .ge. 0. 
            IF(TAOUT(I,K).lt.263.15) THEN
              PGFR(I,K)=SLOR(I,K)**7*FRD1*RONV(I,K)*(DRAIN/RHOd(I,K))*
     1         (EXP(FRA1*(TO-TAOUT(I,K)))-1.)
            ENDIF

C..COLLECTION OF CLOUD ICE BY RAIN, RH (A5), R: (A.41) 
cjmb--PRACI .ge. 0.

            IF(ICE(I,K).GT.R1)THEN
              PRACI(I,K)=CIR*RONV(I,K)*ICE(I,K)*(-0.267*2.
     1         *SLOR(I,K)**3+5.15E3*6.*SLOR(I,K)**4-1.0225E6*24.
     2         *SLOR(I,K)**5+7.55E7*120.*SLOR(I,K)**6)
              PRACI(I,K)=AMAX1(0.,PRACI(I,K))
            ENDIF


C FREEZING OF RAIN BY COLLISION WITH ICE, RH (A7), R: (A.42)
!jmb--  ncon here is N_i/rho in (A42)--13may96
!jmb--  PIACR .ge. 0 according to theory.  The approximation to the collection
!        kernal used is such that for very small RAIN (through SLOR),  negative
!        values can occur.  
!jmb--  Revised calculation to avoid negative values of PIACR when RAIN (SLOR)
!        very small, and to avoid taking such large powers of very small
!        numbers--26may97
            if(ncon(i,k).gt.r1) then
!             New procedure for actually calculating PIACR.
!              Constants defined in PARAMR
              piacr(i,k) = cpiacr0*ronv(i,k)*ncon(i,k)/rhod(i,k) *
     1          (slor(i,k)**6)*
     2          (-1. + cpiacr1*slor(i,k) *
     3          (1.  - cpiacr2*slor(i,k) *
     4          (1.  - cpiacr3*slor(i,k))))
              piacr(i,k) = max(0.,piacr(i,k))
            endif
!           NCAR procedure
!           IF(ICE(I,K).GT.R1)THEN
!             PIACR(I,K)=CIRF*RONV(I,K)*NCON(I,K)/RHOe(I,K)*
!    +         (-0.267*120.*SLOR(I,K)**6+5.15E3*
!    +         720.*SLOR(I,K)**7-1.0225E6*5040.*SLOR(I,K)**8+
!    +         7.55E7*40320.*SLOR(I,K)**9)
!             PIACR(I,K)=AMAX1(0.,PIACR(I,K))
!           ENDIF

cjmb        PSACR and PRACS are results of collisions between rain and snow.
c            PRACS (PSACR) is rate at which rain (snow) collects snow (rain)
c            and
c            is therefore a sink for snow (rain) as result of such
c            collisions (RH).
c            PRACS+PSACR is rate of production of snow + graupel from these
c            collisions, in which the rain is assumed to freeze.
c            ALPHARS, calculated in loop 40, specifies the fraction 
c            converted to snow.--4mar99
cjmb--      For t>0C,
c            PSACR is assumed to result in no additional phase change
c            and does not represent a sink of rainwater.  That is,
c            this process is assumed to have no effect.  For PRACS,
c            the snow collected is assumed to melt and PRACS therefore 
c            contributes to increase rain.--3apr99
!jmb        The constant GAMMA1 = 0.08, set in PARAMR, MM5 version, 
!            [gamma in R (A.47) and (A.48)] is GAMMA3 in FSL version to
!            avoid conflict with GAMMA1 used in CUP.--10feb00 
C COLLECTION OF RAIN BY SNOW, RH (A8)  [R: (A.47)]
            IF(SNOW(I,K).GT.R1)THEN
              PSACR(I,K)=(CSR*RONV(I,K)*SONV(I,K)/RHOd(I,K))*
     1         SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA3*VT2R
     2         *VT2S)*(5.*SLOR(I,K)**6*SLOS(I,K)+2.*SLOR(I,K)**5
     3         *SLOS(I,K)**2+0.5*SLOR(I,K)**4*SLOS(I,K)**3)
C COLLECTION OF SNOW BY RAIN, RH (A9)  [R: (A.48)]
              PRACS(I,K)=(CRS*RONV(I,K)*SONV(I,K)/RHOd(I,K))*
     1         SQRT((ALPHA1*VT2R-BETA1*VT2S)**2+GAMMA3*VT2R
     2         *VT2S)*(5.*SLOS(I,K)**6*SLOR(I,K)+2.*SLOS(I,K)**5
     3         *SLOR(I,K)**2+0.5*SLOS(I,K)**4*SLOR(I,K)**3)
            ENDIF
          ENDIF
          IF(SNOW(I,K).GT.R1)THEN

C...ACCRETION OF CLOUD ICE BY SNOW, DUDHIA: (B15),R: (A.38)
cjmb--      The normalization to son
c            (upper limit zero intercept for snow number concentration)
c            made in loop 20 for sonv is accounted for
c            by including son in the factor ACRIS calculated in PARAMR.
cjmb--      PRAI .ge. 0.
            IF(ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO) THEN
              PRAI(I,K)=ACRIS*SONV(I,K)*SLOS(I,K)**BACRIS*ICE(I,K)
            ENDIF

cjmb--      Next 4 processes involve collisions between snow and cloud 
c            water.
c            PSACW is snow collecting cloud water, a sink for cloud water.
c            Some of the resulting rimed snowflakes (smaller ones)
c            will become graupel, some will remain as
c            rimed snow (still considered snow).
c            The amount of snow so converted to
c            graupel (snow converted to graupel as embryo) is PGEMB.
c            (This only includes what was originally snow, not the 
c            cloud water rimed onto this snow). 
c            PGSACW is that amount of the cloud water collected (rime)
c            that ends up as graupel.
c            PSSACW is the balance of the cloud water collected, 
c            which stays as riming on snow and so
c            adds to snow.  For the procedure specified in R for partitioning
c            between riming augmentation of snow and riming augmentation of 
c            ice, I am assuming that PSSACW should be >= zero.
c            Accordingly, for the case where PGSACW > PSACW, I have 
c            reduced PGSACW.  This ensures conservation of ice mass and
c            eliminates a psbl spurious source of graupel.  For the 
c            Murakami formulation of this partitioning, this issue does
c            not arise.--31mar99 and 31oct00.
COLLECTION OF CLOUD WATER BY SNOW, RH1: (A22)  [R:(A.46)]
            IF(CLOUD(I,K).GT.R1) THEN
              dsno = 4.*slos(i,k)  !  for original size distribution
              stoke_s = dclo*dclo*vt2s*DRAIN/(9.*dum21(i,k)*dsno)
              eff_cs = stoke_s*stoke_s/((stoke_s+0.4)*(stoke_s+0.4))
              eff_cs = max(0.0, min(1.0, eff_cs))
              PSACW(I,K)=ACRCS*eff_cs/EFCS*SONV(I,K)
     1         *SLOS(I,K)**BACRCS*CLOUD(I,K)  ! old size distribution
            ENDIF ! For CLOUD > R1
          ENDIF ! For SNOW > R1

C COLLECTION OF CLOUD WATER BY GRAUPEL, RH (A11)  (R-A.59)
          IF(GRAUPEL(I,K).GT.R1)THEN
Cgt         SLOGN = 0.75785828 * (SLOG(I,K)**0.8) ! Bill hall 3jan01
            SLOGN = SLOG(I,K)                     ! GREG T.  01 Dec 2004
            IF (CLOUD(i,k).GT.R1) THEN
! New Lambda: New Lambda = (4.0*(lambda1)^4)^0.2; where lambda = 1./SLOS
              DGRO = 4.*SLOGN
              stoke_g = dclo*dclo*vt2g*DRAIN/(9.*dum21(i,k)*dgro)
              eff_cg = stoke_g*stoke_g/((stoke_g+0.5)*(stoke_g+0.5))
              eff_cg = max(0.0, min(1.0, eff_cg))
Cgt           PGACW(I,K)=ACRCG_new*GONV(I,K)*EFF_CG/EFGC
Cgt  1         *(SLOGN**BACRCG_new)*CLOUD(I,K)
              PGACW(I,K)=ACRCG*GONV(I,K)*EFF_CG/EFGC
     1         *(SLOGN**BACRCG)*CLOUD(I,K)
            ENDIF
C COLLECTION OF RAIN BY GRAUPEL, RH (A13)  (Missing in R)
            IF (RAIN(I,K).GT.R1) THEN
              PGACR(I,K)=(CRG*RONV(I,K)*GONV(I,K)/RHOd(I,K))
     1         *ABS(VT2G-VT2R)*(5.*SLOR(I,K)**6*SLOG(I,K)+2.
     2         *SLOR(I,K)**5*SLOG(I,K)**2+0.5*SLOR(I,K)**4
     3         *SLOG(I,K)**3)
            ENDIF
          ENDIF

C ICE MULTIPLICATION PROCESS, MRI (11-21)  (R-A.24,25)
cjmb--    Something to think about here: Where is the ice formed via
c          PISPL coming from???? Is it from cloud water????  If so, then
c          not all cloud water collected by snow (PSACW) can be said to 
c          become graupel or to augment snow.  Some must go to cloud ice.
c          Is this adequately accounted for here? -- Apr99
!          Yes--the vrbls PISPL_FS and PISPL_FG (ice splintering with
!          source of ice from snow and from graupel) derived in loop 40
!          correctly account
!          for this by contributing negatively to snow and graupel
!          tendencies.--15feb00 
          IF(PGACW(I,K).GT.0..AND.TAOUT(I,K).GE.265..AND.TAOUT(I,K).LE
     1     .270.) THEN
            TF=0.
            IF(TAOUT(I,K).GE.268..AND.TAOUT(I,K).LE.270.)
     1       TF=(TAOUT(I,K)-270.)/(268.-270.)
            IF(TAOUT(I,K).GE.265..AND.TAOUT(I,K).LT.268.)
     1       TF=(TAOUT(I,K)-265.)/(268.-265.)
Cgt         NI_RS(I,K)=RHOd(I,K)*3.5E8*TF*PSACW(I,K)
            NI_RS(I,K)=RHOd(I,K)*3.5E8*TF*PGACW(I,K)
!           PGACW does not appear here consistent with R:[Eq. A.24) 
            PISPL(I,K)=NI_RS(I,K)*XM01/RHOd(I,K)
          ENDIF

C DEPOSITION OF CLOUD ICE: MRI (11-24)  [R: (A.26)]
          IF (ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO) THEN
            IF(IT.LE.-1.AND.IT.GT.-31)THEN
              A2=ABER2(ABS(IT))
              A1=ABER1(ABS(IT))*(1.*0.001)**(1.-A2)
            ELSEIF(IT.LE.-31)THEN
              A2=ABER2(31)
              A1=ABER1(31)*(1.*0.001)**(1.-A2)
            ENDIF
            PRD(I,K)=(QAOUT(I,K)-QVSI(I,K))/(QQVS(I,K)-QVSI(I,K))
     1       *A1*XMI**A2*NCON(I,K)/RHOd(I,K)
          ENDIF

C...DEPOSITION/SUBLIMATION OF SNOW: DUDHIA (B14)  (R-A.36)
          IF(SNOW(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN
               PREI(I,K)=DEPS1*SONV(I,K)*(QAOUT(I,K)/QVSI(I,K)-1.)
     1              *(0.65*SLOS(I,K)*SLOS(I,K)+((DEPS2*RHOe(I,K)
     2              /DUM21(I,K))**0.5)*DEPS3*0.84*SLOS(I,K)**(DEPS4))
     3              /ABI(I,K) - DUM11(I,K)*PSACW(I,K)

C+---+-----------------------------------------------------------------+ ! GREG T.
C  changed snow to graupel conversion to require riming growth exceed    ! GREG T.
C  depositional growth by factor 3.0 (Murakami required a 1.0 factor     ! GREG T.
C  while RH1984 required qc>0.5 and qs>0.1 - similar to 5.0 factor)      ! GREG T.
               if (psacw(i,k).gt.3.0*prei(i,k)) then                     ! GREG T.
                  pgemb(i,k)=dsnow/(dgraupel-dsnow) * psacw(i,k)         ! GREG T.
                  pgsacw(i,k)=psacw(i,k)-pgemb(i,k)                      ! GREG T.
                  pssacw(i,k)=0.                                         ! GREG T.
               else                                                      ! GREG T.
                  pgemb(i,k)=0.                                          ! GREG T.
                  pgsacw(i,k)=0.                                         ! GREG T.
                  pssacw(i,k) = psacw(i,k)                               ! GREG T.
               endif                                                     ! GREG T.
C+---+-----------------------------------------------------------------+ ! GREG T.
          ENDIF

C SUBLIMATION/DEPOSITIONAL GROWTH OF GRAUPEL, RH (A17)  (R-A.57)
!         IF(GRAUPEL(I,K).GT.R1 .AND. TAOUT(I,K).LT.TO) THEN
          IF(GRAUPEL(I,K).GT.1.E7 .AND. TAOUT(I,K).LT.TO) THEN ! 29dec04-Greg T

!           New Lambda: New Lambda = (4.0*(lambda1)^4)^0.2; 
!            where lambda = 1./SLOS
cc          SLOGN = 0.75785828 * (SLOG(I,K)**0.8) ! Bill Hall 3jan01
Cgt         PREG(I,K)=DEPG1*GONV(I,K)*(QAOUT(I,K)/QVSI(I,K)-1.)
Cgt  1       *(0.78*SLOGN**3+((DEPG2*RHOe(I,K)
Cgt  2       /DUM21(I,K))**0.5)*.31*4.098*SLOGN**(DEPG4+1.))
Cgt  3       /ABI(I,K) - DUM11(I,K)*PGACW(I,K)
            PREG(I,K)=DEPG1*GONV(I,K)*(QAOUT(I,K)/QVSI(I,K)-1.)
     1       *(0.78*SLOGN*SLOGN+((DEPG2*RHOe(I,K)
     2       /DUM21(I,K))**0.5)*DEPG3*SLOGN**(DEPG4))
     3       /ABI(I,K) - DUM11(I,K)*PGACW(I,K)
          ENDIF
          FUDGEF = 0.95
          SUM_DEP = PRI(I,K)+PRD(I,K)+PREI(I,K)+PREG(I,K)
          IF( (SUPICE.GT.0. .AND. SUM_DEP.GT.SUPICE*FUDGEF) .
     1     OR. (SUPICE.LT.0. .AND. SUM_DEP.LT.SUPICE*FUDGEF) ) THEN
            PRI(I,K) = FUDGEF*PRI(I,K)*SUPICE/SUM_DEP
            PRD(I,K) = FUDGEF*PRD(I,K)*SUPICE/SUM_DEP
            PREI(I,K) = FUDGEF*PREI(I,K)*SUPICE/SUM_DEP
            PREG(I,K) = FUDGEF*PREG(I,K)*SUPICE/SUM_DEP
          ENDIF

C RIMING OF CLOUD ICE, MRI (11-25A)   [R: (A.27,28a]
cjmb--THis considerably chgd frm pre 1998 versions.  Check it out (????)
c         Note of clarification: PIACW1 is the collection of cloud water
!          by cloud ice (i.e., riming on cloud ice).
!          Two cases for conversion of this to graupel.
!          1. PIACW1 < PRDTEMP: riming on cloud ice less than deposition
!             of cloud ice.  In this case, all riming goes to increasing
!             mixing ratio of cloud ice (PIACW = PIACW1 and PGIACW = 0).
!          2. PIACW1 > PRDTEMP: Riming on cloud ice exceeds deposition.
!             The excess contributes to graupel (PIACW = PRDTEMP
!             and PGIACW = PIACW1 - PRDTEMP).
!          Either way, PIACW + PGIACW = PIACW1, the loss of cloud water
!          to riming (see loop 40).  
          UDCLO=3.E7*DCLO*DCLO
          RPHI=DCLO*(DRAIN*VT2I/(3.24e-4*DIACE))**0.5
Cgt       IF(RPHI.GE.0.2704.AND.CLOUD(I,K).GT.R1.AND
Cgt  1     . ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN
          IF(RPHI.GE.0.2704.AND.CLOUD(I,K).GT.R1.AND                     ! GREG T.
     1     . DIACE.GT.100.E-6.AND.TAOUT(I,K).LT.TO)THEN                  ! GREG T.
            EIC1=0.572*ALOG10(RPHI-0.25)+0.967
            EIC1=AMIN1(EIC1,0.5)                                         ! GREG T.
            PIACW1=PI/4.*NCON(I,K)*(DIACE+DCLO)**2*EIC1  
     1       *ABS(VT2I-UDCLO)*CLOUD(I,K)
            PRDTEMP=AMAX1(PRD(I,K),0.0)
            PIACW(I,K)=AMIN1(PIACW1,PRDTEMP)

C AMOUNT OF RIME CONVERTED INTO GRAUPEL, MRI (11-38)  [R: (A.28B)]
            PGIACW(I,K) = AMAX1(0.0, PIACW1-PRDTEMP)

            CLIC=AMAX1((PIACW1-PRDTEMP)/NCON(I,K),0.0)

C AMOUNT OF CLOUD ICE CONVERTED INTO GRAUPEL, MRI (11-36)  [R: (A.39)]
            DMGI=XM0G-XMI
            IF(DMGI.GT.0.)THEN
              PICNG(I,K)=CLIC*ICE(I,K)*RHOd(I,K)/DMGI
            ENDIF

C REDUCE NUMBER CONC OF CLOUD ICE SINCE CONVERTED INTO GRAUPEL  [R: (A.40)]
            NI_CG(I,K)=RHOd(I,K)/XM0G*(PICNG(I,K)+PGIACW(I,K))
          ENDIF
          IF(ICE(I,K).GT.R1.AND.TAOUT(I,K).LT.TO)THEN
C CONVERSION OF CLOUD ICE TO SNOW, MRI (11-30)  [R: (A.30a,30b)]
            DEPAC = 0.
            IF (PRD(I,K).GT.0.0) THEN
              IF(XMI.LE.0.5*XM0S)THEN
                DEPAC=XMI/(XM0S-XMI)*(PRD(I,K)+PIACW(I,K))
              ELSE
                DEPAC=PRD(I,K)+PIACW(I,K)+(1.-0.5*XM0S/XMI)
cjmb 1           *ICE(I,K)/(2.*DT)                                ! GREG T.
     1           *ICE(I,K)/DT ! BUGFIX--DT is interval bet EXMOISG calls 
              ENDIF
C REDUCE NI DUE TO ICE GROWING TO SNOW SIZES
              NI_DE(I,K) = DEPAC*(RHOd(I,K)/XM0S)
            ELSE
C REDUCE NI DUE TO EVAPORATION (MISSING IN R reference)
               NI_EV(I,K) = -PRD(I,K)*(RHOd(I,K)/XMI)
            ENDIF
            PSAGI = 0.

C REDUCE NI DUE TO SMALL CRYSTALS COLLIDING PRODUCE LARGER ONES (STILL ICE)
C THIS IS MISSING IN REISNER PAPER BUT IS IN MRI (M-39)
Cgt         C1_AG = C1*RHOd(I,K)*ICE(I,K)*RHO_FAC(I,K)
            C1_AG = PI/6.*0.1*0.25*VT2I*DIACE*DIACE               ! GREG T.
Cgt         NI_AG(I,K) = 0.5*C1_AG*NCON(I,K)
            NI_AG(I,K) = 0.5*C1_AG*NCON(I,K)*NCON(I,K)            ! GREG T.
            IF (DIACE.LT.2.*XR0S) THEN
Cgt           DTAU=-2.0/C1_AG*ALOG10((DIACE/(2.*XR0S))**3)
              DTAU=-2.0/(C1_AG*NCON(I,K))                         ! GREG T.
     +                       *ALOG10((DIACE/(2.*XR0S))**3)        ! GREG T.
              PSAGI=ICE(I,K)/DTAU
            ENDIF
c****Figure out what exactly PSCNI is doing--chk MRI
            PSCNI(I,K)=DEPAC+PSAGI

C ADD TO ABOVE AMOUNT OF AGGREGATION THAT CONVERTS ICE TO SNOW [R: (A.31)]
            NI_AG(I,K) = NI_AG(I,K) + PSAGI*(RHOd(I,K)/XMI)
          ENDIF

          IF (TAOUT(I,K).GT.TO) THEN
C REMEMBER THE SIGNS ARE REVERSED TO-TAOUT
C GO BACK AND CHECK FOR CONSERVATION


!jmb        RH1 points out that the effect on melting of water vapor
!            condensation on snow is ignored.
!            What about the opposite effect, of evaporation
!            from melting snow on cooling down the snow particles?  
!            According to Roelof (19sep96), this should be included, 
!            but is ignored for simplicity.
!            To his knowledge, no complete analysis
!            of this has been done.  But see a manuscript or preprint by 
!            Istvan Geresdi in my files.
            XLATF=XLS-DUM31(I,K)
            IF(SNOW(I,K).GT.R1) THEN
C MELTING OF SNOW, RH1 (A23) [R: (A.37)]
              PSMLT(I,K)=PSM1*SONV(I,K)*SCR4(I,K)/XLATF*
     1         (TO-TAOUT(I,K))*(0.65*SLOS(I,K)*SLOS(I,K)+
     2         ((PSM2*RHOe(I,K)/DUM21(I,K))**0.5)*PSM3
     3         *SLOS(I,K)**PSM4)

C EVAPORATION OF MELTING SNOW FOR T > TO; RH1 (A27) 
              IF(QAOUT(I,K)-QQVS(I,K).LT.0.)THEN
                PMLTEV(I,K)=DEPS1*SONV(I,K)*(QAOUT(I,K)/QQVS(I,K)
     1           -1.)*(0.65*SLOS(I,K)*SLOS(I,K)+((DEPS2
     2           *RHOe(I,K)/DUM21(I,K))**0.5)*DEPS3*0.84
     3           *SLOS(I,K)**(DEPS4))/ABW(I,K)
              ENDIF
            ENDIF

            IF(GRAUPEL(I,K).GT.R1)THEN
C MELTING OF GRAUPEL, RH (A18); R:(A.58)
              PGMLT(I,K)=PGM1*GONV(I,K)*SCR4(I,K)/XLATF*
     1         (TO-TAOUT(I,K))*(0.78*SLOG(I,K)*SLOG(I,K)+
     2         ((PGM2*RHOe(I,K)/DUM21(I,K))**0.5)*PGM3
     3         *SLOG(I,K)**PGM4)

C EVAPORATION OF MELTING GRAUPEL, RH (A19)
              IF(QAOUT(I,K)-QQVS(I,K).LT.0.)THEN
                PMLTGE(I,K)=DEPG1*GONV(I,K)*(QAOUT(I,K)
     1           /QQVS(I,K)-1.)*(0.78*SLOG(I,K)*SLOG(I,K)
     2           +((DEPG2*RHOe(I,K)/DUM21(I,K))**0.5)*DEPG3
     3           *SLOG(I,K)**(DEPG4))/ABW(I,K)
              ENDIF

C SHEDDING OF ACCRETED WATER, RH (A20)
C SUM OF PGACR(I,K) AND PGACW(I,K) ADDED TO TEND

C ENHANCED MELTING OF GRAUPEL BY COLLECTION OF RAIN WATER, RH (A21)
C ENHANCED MELTING OF GRAUPEL BY COLLECTION OF CLOUD WATER, RH (A21)
C SIGNS ARE REVERSED (TO-TAOUT), CW (PARAMR) is specific heat
              PGACRM(I,K)=CW/XLATF*(TO-TAOUT(I,K))*PGACR(I,K)

C SIGNS ARE REVERSED
              PGACWM(I,K)=CW/XLATF*(TO-TAOUT(I,K))*PGACW(I,K)

            endif  ! For graupel > R1

          ENDIF  ! For TAOUT > 273.15
  702     CONTINUE
 30   CONTINUE 
c     enddo ! loop 30


      DO 40 K=1,KL
!       mm5 is k (upside down), RUC is kk 
        kk=kl-k+1
        DO 40 I= ist,ien
          PRE(I,K)=AMAX1(-RAIN(I,K)/DT,PRE(I,K))
          PREI(I,K)=AMAX1(-SNOW(I,K)/DT,PREI(I,K))
          PREG(I,K)=AMAX1(-GRAUPEL(I,K)/DT,PREG(I,K))
          PRD(I,K)=AMAX1(-ICE(I,K)/DT,PRD(I,K))
C CHECKS TO SEE WHETHER GRAUPEL OR SNOW IS FORMED, SEE RH  (R-A.51)
cc        ALPSNOW=AMAX1(1.E-25,DSNOW**2*(4.*SLOS(I,K))**6) ! MM5
          ALPSNOW=AMAX1(1.E-25,DSNOW2*(4.*SLOS(I,K))**6)   ! FSL 
cc        ALPRAIN=AMAX1(1.E-25,DRAIN**2*(4.*SLOR(I,K))**6) ! MM5
          ALPRAIN=AMAX1(1.E-25,DRAIN2*(4.*SLOR(I,K))**6)   ! FSL
          ALPHARS=ALPSNOW/(ALPSNOW+ALPRAIN)
Cgt       ALPHARS=0.0
c          D1=1.
c          IF(RAIN(I,K).GT.0.0001) D1=0.

c         IF (RAIN(I,K).LT.0.0005) then ! older 
!         IF (RAIN(I,K).LT.0.0001) then ! this is frm ncar 19jan00 version
!           THETAh = PI - PI*.5*(0.0001-RAIN(I,K))/0.0001
!           D1 = SIN(THETAh)
!         ELSE
!           D1 = 0.
!         ENDIF
!         More efficient way (checked out 11feb00):
          d1 = 1.
          IF (RAIN(I,K).GT.R1) then
            thetah = 0.5*pi*(1. - 1.e4*min(rain(i,k),0.0001))
            d1 = sin(thetah)
          endif
!         End more efficient way 
cjmb--    In the preceeding, the formula (A.51) of R is not used.  

!jmb--    For each water-substance specie (except water vapor) sum up the terms
!          (into variable DUM11) that affect that specie and then constrain 
!          the sum of these terms, integrated over a time step,
!          to not so drastically deplete this specie
!          that its mixing ratio becomes negative.  This constraint is imposed
!          by multiplying each process rate by a factor (RATIO) no larger
!          than unity.  Once this is done, MM5 tendencies are computed.
!          Do this in 2 sections, one for temps at or below freezing, 
!                               other for temps above       freezing.

!jmb--    Should tendencies from convection (loop 17) be included in these
!          constraints???? Leave out for now.--22may00 

          IF(TAOUT(I,K).LE.TO) THEN
cjmb--      For temperature below freezing

CONSERVATION OF QC
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+PGACW(I,K)+
     1       PIFCW(I,K)+PIACW(I,K)+PGIACW(I,K))*DT
            IF((DUM11(I,K).GE.CLOUD(I,K)) .AND. (CLOUD(I,K).GT.R1)) THEN       
c             For consistency with adjustment of PSACW 
c              [PSACW = PSSACW + PGSACW, see (A45) in R], 
c              also adjust PSSACW and PGSACW.  Also need to adjust PGEMB
c              since it depends on PGSACW.  It also make sense to
c              adjust PISPL since it is proportional to PSACW+PGACW--20Jan98
c              3apr99, and 17oct00.  Note, when went to Murakami procedure
c              for riming of snow, NCAR felt that PISPL should no longer
c              be adjusted.  
              ratio = cloud(i,k)/dum11(i,k)
              PIFCW(I,K) = ratio*PIFCW(I,K)
              PRC(I,K)   = ratio*PRC(I,K)
              PRA(I,K)   = ratio*PRA(I,K)
              PSACW(I,K) = ratio*PSACW(I,K)
              PGSACW(i,k)= ratio*PGSACW(i,k)
              PSSACW(I,K)= ratio*PSSACW(I,K)
              PGEMB(I,K) = ratio*PGEMB(I,K)        
              PGACW(I,K) = ratio*PGACW(I,K)
              PIACW(I,K) = ratio*PIACW(I,K)
Cgt           PISPL(I,K) = ratio*PISPL(I,K)
              PGIACW(I,K)= ratio*PGIACW(I,K)
            ENDIF

CONSERVATION OF CLOUD ICE
            DUM11(I,K)=(PSCNI(I,K)+PRAI(I,K)-PRI(I,K)-PRD(I,K)
     1       +PRACI(I,K)-PIFCW(I,K)-PIACW(I,K)
     2       +PICNG(I,K)-PISPL(I,K))*DT
            IF(DUM11(I,K).GE.ICE(I,K).and.ice(i,k).gt.R1)THEN
              ratio = ice(i,k)/dum11(i,k)
              PSCNI(I,K) =  ratio*PSCNI(I,K)
              PRAI(I,K)  =  ratio*PRAI(I,K)
              PRI(I,K)   =  ratio*PRI(I,K)
              PRD(I,K)   =  ratio*PRD(I,K)
              PRACI(I,K) =  ratio*PRACI(I,K)
              PIFCW(I,K) =  ratio*PIFCW(I,K)
              PIACW(I,K) =  ratio*PIACW(I,K)
              PICNG(I,K) =  ratio*PICNG(I,K)
              PISPL(I,K) =  ratio*PISPL(I,K)
            ENDIF

CONSERVATION OF RAIN
            DUM11(I,K)=(PIACR(I,K)+PSACR(I,K)+PGACR(I,K)-PRC(I,K)
     1       -PRA(I,K)+PGFR(I,K)-PRE(I,K))*DT
cjmb--      PRE < 0 for evaporation of rain.
!            DUM11 is positive for depletion of rain.
!            Thus, since PRE is negative for evaporation (contributing toward
!            depletion) of rain, the sign in DUM11 calculation shud be minus.
            IF(DUM11(I,K).GE.RAIN(I,K).AND.RAIN(I,K).GT.R1)THEN
              ratio = rain(i,k)/dum11(i,k)
              PIACR(I,K) = ratio*PIACR(I,K)
              PSACR(I,K) = ratio*PSACR(I,K)
              PGACR(I,K) = ratio*PGACR(I,K)
              PGFR(I,K)  = ratio*PGFR(I,K)
              PRC(I,K)   = ratio*PRC(I,K)
              PRA(I,K)   = ratio*PRA(I,K)
              PRE(I,K)   = ratio*PRE(I,K)
            ENDIF

CONSERVATION OF SNOW
            DUM11(I,K)=(PRACS(I,K)*(1.-ALPHARS)+PGEMB(I,K)
     1       -PREI(I,K)-PSCNI(I,K)-PRAI(I,K)-
     2       PIACR(I,K)*D1-PRACI(I,K)*D1-PSACR(I,K)*ALPHARS-
     3       PSSACW(I,K))*DT
            IF(DUM11(I,K).GE.SNOW(I,K).AND.SNOW(I,K).GT.R1)THEN
      
              ratio=snow(i,k)/dum11(i,k)
              PRACS(I,K)    = ratio*PRACS(I,K)
              PGEMB(I,K)    = ratio*PGEMB(I,K)
              PSCNI(I,K)    = ratio*PSCNI(I,K)
              PRAI(I,K)     = ratio*PRAI(I,K)
              PIACR(I,K)    = ratio*PIACR(I,K)
              PRACI(I,K)    = ratio*PRACI(I,K)
              PSACR(I,K)    = ratio*PSACR(I,K)
              PSSACW(I,K)   = ratio*PSSACW(I,K)
              PREI(I,K)     = ratio*PREI(I,K)
            ENDIF

CONSERVATION OF GRAUPEL
c           **** Check this and graupel tendency carefully--
!            taken frm the 19jan00 version frm Kevin.--2feb00.
            DUM11(I,K)=-(PGACW(I,K)+PGACR(I,K)+
     1       PREG(I,K)+(1.-D1)*PRACI(I,K)+(1.-D1)*
     2       PIACR(I,K)+(1.-ALPHARS)*PSACR(I,K)+(1.-ALPHARS)*
     3       PRACS(I,K)+PGEMB(I,K)+PGSACW(I,K)+
     4       PGFR(I,K)+PICNG(I,K)+PGIACW(I,K))*DT

            IF(DUM11(I,K).GE.GRAUPEL(I,K).AND.GRAUPEL(I,K).GT.R1)THEN
              ratio = graupel(i,k)/dum11(i,k)
              PGACW(I,K)= ratio*PGACW(I,K)
              PGACR(I,K)= ratio*PGACR(I,K)
              PREG(I,K) = ratio*PREG(I,K)
              PRACI(I,K)= ratio*PRACI(I,K)
              PIACR(I,K)= ratio*PIACR(I,K)
              PSACR(I,K)= ratio*PSACR(I,K)
              PRACS(I,K)= ratio*PRACS(I,K)
              PGEMB(I,K)= ratio*PGEMB(I,K)
              PGSACW(I,K)=ratio*PGSACW(I,K)
              PGFR(I,K)=  ratio*PGFR(I,K)
              PICNG(I,K)= ratio*PICNG(I,K)
              PGIACW(I,K)=ratio*PGIACW(I,K)
            ENDIF

!jmb--      Next if block accounts for the cloud water that contributes to
!            ice splintering by assuming that a fraction of the
!            cloud water collected by both snow (PSACW) and by graupel (PGACW)
!            does not actually go to snow and graupel, but to cloud ice 
!            "splinters" instead.  This answers a bug in mass budget
!            discovered Apr99--15feb00
Cgt         psacw(i,k) = pssacw(i,k) + pgsacw(i,k)
            psacw(i,k) = pssacw(i,k) + pgsacw(i,k) + pgemb(i,k)
cjmb--      Preceeding added for consistency since pssacw and pgsacw may
c            be adjusted independently.--31oct00
            PISPL_FS = 0.
            PISPL_FG = 0.
            IF (PISPL(I,K).GT.R1 .AND.
     .       (PSACW(I,K)+PGACW(I,K)).GT.0.) THEN
              PISPL_FS=PISPL(I,K)*(PSACW(I,K)/(PSACW(I,K)+PGACW(I,K)))
              PISPL_FG=PISPL(I,K)*(PGACW(I,K)/(PSACW(I,K)+PGACW(I,K)))
            ELSE
              PISPL(I,K) = 0.
            ENDIF

cjmb--In CHANGTQ, when these tendencies are converted to MAPS tendencies, 
c      is the same value of PSB used????

C WATER VAPOR TENDENCY
            QVTEN(I,K)=QVTEN(I,K)-(PRE(I,K)+PREI(I,K)+PRI(I,K)
     1       +PRD(I,K)+PREG(I,K))*PSB(I,J)

C CLOUD WATER TENDENCY
            QCTEN(I,K)=QCTEN(I,K)-(PRC(I,K)+PRA(I,K)+PIACW(I,K)+
     1       PSACW(I,K)+PGACW(I,K)+PIFCW(I,K)+
     2       PGIACW(I,K))*PSB(I,J)
 
C RAIN TENDENCY
            QRTEN(I,K)=QRTEN(I,K)+(PRC(I,K)+PRA(I,K)+PRE(I,K)-
     1       PGACR(I,K)-PSACR(I,K)-PIACR(I,K)-PGFR(I,K))*PSB(I,J)

C CLOUD ICE TENDENCY
            QITEN(I,K)=QITEN(I,K)-(PSCNI(I,K)+PRAI(I,K)-PRI(I,K)
     1       -PRD(I,K)+PRACI(I,K)-PIFCW(I,K)
     2       -PIACW(I,K)+PICNG(I,K)-PISPL(I,K))*PSB(I,J)

C SNOW TENDENCY
            QNITEN(I,K)=QNITEN(I,K)+(PREI(I,K)+PSCNI(I,K)+PRAI(I,K)
     1       +D1*PRACI(I,K)+D1*PIACR(I,K)+ALPHARS*PSACR(I,K)-  
     2       (1.-ALPHARS)*PRACS(I,K)+PSSACW(I,K)
     3       -PGEMB(I,K)-PISPL_FS)*PSB(I,J)

C GRAUPEL TENDENCY
            QGTEN(I,K)=QGTEN(I,K)+(PGACW(I,K)-PISPL_FG+PGACR(I,K)
     1       +PREG(I,K)+(1.-D1)*PRACI(I,K)+(1.-D1)
     2       *PIACR(I,K)+(1.-ALPHARS)*PSACR(I,K)+(1.-ALPHARS)
     3       *PRACS(I,K)+PGEMB(I,K)+PGSACW(I,K)
     4       +PGFR(I,K)+PICNG(I,K)+PGIACW(I,K))*PSB(I,J)

cjmb--      Why does PGSACW not appear in any other equations????
c            PSSACW = PSACW - PGSACW.
cjmb         Note that PSACW aprs with minus sign in
c            QCTEN, and PSSACW is positive in QNITEN.
!            So, with these substitutions, there is conservation.--31mar99
 
C NUMBER OF CLOUD ICE TEND
C QNCTEN originally #/M3.  Dec 03: Changed to #/kg.

Cgt         QNCTEN(I,K)=QNCTEN(I,K)+((PRI(I,K)+PIFCW(I,K))  ! Dec 03
Cgt  1       *RHOd(I,K)/XM01-(PRAI(I,K)+PRACI(I,K))         ! Dec 03
Cgt  2       *NCON(I,K)/ICE(I,K)+NI_RS(I,K)-NI_CG(I,K)      ! Dec 03
Cgt  3       -NI_AG(I,K)-NI_DE(I,K)-NI_EV(I,K))*PSB(I,J)    ! Dec 03
            QNCTEN(I,K)=QNCTEN(I,K)+(((PRI(I,K)+PIFCW(I,K)) 
     1       *RHOd(I,K)/XM01-(PRAI(I,K)+PRACI(I,K))         
     2       *NCON(I,K)/ICE(I,K)+NI_RS(I,K)-NI_CG(I,K)     
     3       -NI_AG(I,K)-NI_DE(I,K)-NI_EV(I,K))*PSB(I,J))/RHOd(i,k) 

C SET CONSTRAINTS [R:(A.68)] (Number concentration of ice constrained to
!                             be consistent with assumed min mass of ice 
!                             particle and predicted mixing ratio of ice)
!mm5        TEMP_ICE = AMAX1(R1,(QIB(I,J,K)+DT*QITEN(I,K))
!mm5 +       *RPSB(I,J))
            TEMP_ICE = AMAX1(R1,(QIA(I,J,Kk)+(DT/psb(i,j))*QITEN(I,K)))
!mm5        TEMP_NC  = AMAX1(R1,(QNCB(I,J,K)+DT*QNCTEN(I,K))
!mm5 +       *RPSB(I,J))
            TEMP_NC  = AMAX1(R1,
     1       (QNCA(I,J,Kk)+(DT/PSB(I,J))*QNCTEN(I,K)))       
            IF (TEMP_ICE.GT.R1) THEN   
Cgt           RLU=RHOd(I,K)*TEMP_ICE/XM01 ! Dec 03
              RLU=TEMP_ICE/XM01 
!mm5          IF(TEMP_NC.GT.RLU) QNCTEN(I,K)=(PSB(I,J)*RLU
!mm5 +                 - QNCB(I,J,K))/DT
              IF(TEMP_NC.GT.RLU) QNCTEN(I,K)=(PSB(I,J)/DT)*
     1         (RLU - QNCA(I,J,Kk))
            ELSEIF (TEMP_ICE.LE.R1 .AND. TEMP_NC.GT.R1) THEN
              QNCTEN(I,K) = -1.*(TEMP_NC*(PSB(I,J)/DT))
            ENDIF

            XLATF=XLS-DUM31(I,K)
            TEMP=-XLS*(PREI(I,K)+PRD(I,K)+PRI(I,K)+PREG(I,K))
     1       *PSB(I,J)-DUM31(I,K)*PRE(I,K)*PSB(I,J)-XLATF
     2       *(PSACW(I,K)+PIACR(I,K)+PSACR(I,K)+PGACR(I,K)
     3       +PGACW(I,K)+PIFCW(I,K)+PIACW(I,K)+PGFR(I,K)
     4       +PGIACW(I,K))*PSB(I,J)

!maps       TEMP=-XLS*(PREI(I,K)+PRD(I,K)+PRI(I,K)+PREG(I,K))*PSB(I,J)
!maps1       -DUM31(I,K)*PRE(I,K)*PSB(I,J)
!maps2       -XLATF*(PSACW(I,K)+PIACR(I,K)+PSACR(I,K)+
!maps3            PGACR(I,K)+PGACW(I,K)+PIFCW(I,K)+PIACW(I,K)+
!maps4            PGFR(I,K)+PGIACW(I,K))*PSB(I,J)
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))
cjmb--The 1 + .887*qaout in the above is correction to specific
cjmb   heat at const pres to allow for presence of water vapor.  
cjmb   Source:  Bolton (1980) 
            TTEN(I,K)=TTEN(I,K)-TEMP/PRD(I,K)
cmm5           TTEN(I,K)=TTEN(I,K)-TEMP/PRD(I,K)*(1-IFDRY)

C+---+-----------------------------------------------------------------+
C-- ABOVE 0 C CASE BELOW HERE
C+---+-----------------------------------------------------------------+

          ELSE

CONSERVATION OF QC
            DUM11(I,K)=(PRC(I,K)+PRA(I,K)+PSACW(I,K)+PGACW(I,K))*DT
            IF(DUM11(I,K).GE.CLOUD(I,K).AND.CLOUD(I,K).GT.R1)THEN
              ratio=cloud(i,k)/dum11(i,k)
              PRC(I,K)   = ratio*PRC(I,K)
              PRA(I,K)   = ratio*PRA(I,K)
              PSACW(I,K) = ratio*PSACW(I,K)
              PGACW(I,K) = ratio*PGACW(I,K)
            endif

CONSERVATION OF QR

            DUM11(I,K)=-(PRC(I,K)+PRA(I,K)+PRE(I,K)-PGMLT(I,K)
     1       -PSMLT(I,K)-PGACRM(I,K)-PGACWM(I,K)+PSACW(I,K)+
     2       PRACS(I,K)+PGACW(I,K))*DT

            IF(DUM11(I,K).GE.RAIN(I,K).AND.RAIN(I,K).GT.R1)THEN
              ratio = rain(i,k)/dum11(i,k)
              PRC(I,K)=RAtIo*PRC(I,K)
              PRA(I,K)=RAtIo*PRA(I,K)
              PRE(I,K)=RAtIo*PRE(I,K)
              PGMLT(I,K)=RAtIo*PGMLT(I,K)
              PSMLT(I,K)=RAtIo*PSMLT(I,K)
              PGACRM(I,K)=RAtIo*PGACRM(I,K)
              PGACWM(I,K)=RAtIo*PGACWM(I,K)
              PSACW(I,K)=RAtIo*PSACW(I,K)
              PRACS(I,K)=RAtIo*PRACS(I,K)
              PGACW(I,K)=RAtIo*PGACW(I,K)
            ENDIF
            IF(IIWARM)GOTO 703

CONSERVATION OF SNOW
            DUM11(I,K)=(PRACS(I,K)-PMLTEV(I,K)-PSMLT(I,K))*DT
            IF(DUM11(I,K).GE.SNOW(I,K).AND.SNOW(I,K).GT.R1)THEN
              ratio=snow(i,k)/dum11(i,k)
              PMLTEV(I,K) = ratio*PMLTEV(I,K)
              PSMLT(I,K)  = ratio*PSMLT(I,K)
              PRACS(I,K)  = ratio*PRACS(I,K)
            ENDIF

CONSERVATION OF GRAUPEL
            DUM11(I,K)=(-PGMLT(I,K)-PGACRM(I,K)-PGACWM(I,K)
     1       -PMLTGE(I,K))*DT
            IF(DUM11(I,K).GE.GRAUPEL(I,K).AND.GRAUPEL(I,K).GT.R1)THEN
              ratio=graupel(i,k)/dum11(i,k)
              PGMLT(I,K)  = ratio*PGMLT(I,K)
              PGACRM(I,K) = ratio*PGACRM(I,K)
              PGACWM(I,K) = ratio*PGACWM(I,K)
              PMLTGE(I,K) = ratio*PMLTGE(I,K)
            ENDIF 
  703       CONTINUE

C WATER VAPOR TENDENCY
            QVTEN(I,K)=QVTEN(I,K) -
     1       (PRE(I,K)+PMLTEV(I,K)+PMLTGE(I,K))*PSB(I,J)

C CLOUD WATER TENDENCY
            QCTEN(I,K)=QCTEN(I,K) -
     1       (PRC(I,K)+PRA(I,K)+PSACW(I,K)+PGACW(I,K))*PSB(I,J)
 
C RAIN TENDENCY
            QRTEN(I,K)=QRTEN(I,K)+(PRC(I,K)+PRA(I,K)
     1       +PRE(I,K)-PGMLT(I,K)-PSMLT(I,K)-PGACRM(I,K)
     2       -PGACWM(I,K)+PSACW(I,K)+PRACS(I,K)
     3       +PGACW(I,K))*PSB(I,J)
!jmb--      Note that it is assumed in the above that cloud water collected 
!           by snow (PSACW) and snow collected by rain (PRACS) contribute 
!           to rain when temp above freezing.
!           This means that PRACS also represents an additional
!           melting of snow.--5mar99

            IF(IIWARM)GOTO 704


C SNOW TENDENCY
            QNITEN(I,K)=QNITEN(I,K)+(PSMLT(I,K)
     1       -PRACS(I,K)+PMLTEV(I,K))*PSB(I,J)

C GRAUPEL TENDENCY
            QGTEN(I,K)=QGTEN(I,K)+(PGMLT(I,K)
     2       +PGACRM(I,K)+PGACWM(I,K)+PMLTGE(I,K))*PSB(I,J)

  704       CONTINUE
      
            XLATF=XLS-DUM31(I,K) ! Latent heat of melting
            TEMP=-XLATF*(PGMLT(I,K)+PSMLT(I,K)+PGACWM(I,K)
     1       +PGACRM(I,K)-PRACS(I,K))*PSB(I,J)
     2       -DUM31(I,K)*(PRE(I,K)+PMLTEV(I,K)+PMLTGE(I,K))*PSB(I,J)
            PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))
            TTEN(I,K)=TTEN(I,K)-TEMP/PRD(I,K)
cmm5        TTEN(I,K)=TTEN(I,K)-TEMP/PRD(I,K)*(1-IFDRY)

          ENDIF ! End above 0C case

!     enddo
   40 CONTINUE

CLOUD NEXT PART IS TO GET RID OF EXCESS WATER VAPOR IF
C SS WITH RESPECT TO WATER, AS IN ANTHES AND WARNER
C FOLLOWS EXMOISS OF DUDHIA WITH NO MODIFICATION

C---COMPUTE T, QV, AND QC AT TAU+1 WITHOUT CONDENSATIONAL TERM:
cjmb****Are we making any systematic errors by doing the saturation
c        adjustment here rather than separately after COORD is called
c        (i.e., after vertical advection)????--31oct00
cjmb    I do not think so.  This saturation adjustment will be a primary
c        source of latent heat release or absorption that shud be taken into
c        acct in coordinate adjustment.  It is psbl that RH will not be 
c        precisely 100% in regions of condensation after COORD is called,
c        the deficiency will be accounted for in the next time step.--15dec00  

      DO 50 K=1,KL
        kk=kl-k+1
!     --k is MM5, kk is maps
        DO 50 I=ist,ien
          opsb = 1./psb(i,j)
cjmb--    Note that here qvb is a maps, not an MM5, variable.  Use inverted
c          subscript and dont divide qvb by pstar.
cc        DUM11(I,K)=QVb(I,J,kk)+DT*QVTEN(I,K)/psb(i,j)
          DUM11(I,K)=QVa(I,J,kk)+DT*QVTEN(I,K)*opsb
          SCR4(I,K)=AMAX1(1.E-12,DUM11(I,K))
cjmb      DUM21(I,K)=(QCb(I,J,K)+DT*QCTEN(I,K))*opsb
          DUM21(I,K)=QCa(I,J,kk)+DT*QCTEN(I,K)*opsb
          SCR3(I,K)=AMAX1(0.,DUM21(I,K))

          SCR7(I,K)=Taout(I,K) + DT*TTEN(I,K)*opsb  
!jmb--    SCR7 is temperature after microphysical changes incorporated 
!          (see below where it is used in saturation vapor pressure formula.)

   50 CONTINUE

C----COMPUTE THE CONDENSATIONAL TERM:

!jmb--Loop 60 computes saturation mixing ratio over water for saturation 
!      adjustment.

      DO 60 K=1,KL
        DO 60 I=ist,ien
          PRD(I,K)=CP*(1.+0.887*QAOUT(I,K))
          DUM21(I,K)=DUM31(I,K)*DUM31(I,K)/(RV*PRD(I,K))

cjmb--    Saturation vapor pressure in pascals.
c         Should replace this by lookup tables
Cgt       DUM11(I,K)=1.E3*SVP1*EXP(SVP2*(SCR7(I,K)-SVPT0)
Cgt  1     /(SCR7(I,K)-SVP3))

cjmb--    Saturation mixing ratio
Cgt       PRC(I,K)=.62198*DUM11(I,K)/(PREs(I,K)-DUM11(I,K))
          PRC(I,K)=RSLF(PRES(I,K),SCR7(I,K))                             ! GREG T.
 60   CONTINUE

cjmb--saturation adjustment in loop 70
      DO 70 K=1,KL
        kk=kl-k+1
        DO 70 I=ist,ien
          R2 = R1*PSB(I,J)
!jmb--    At start of this loop:
!          scr3  = predicted cloud water mixing ratio at n+1 before adjustment.
!          scr4  = predicted vapor mixing ratio at time step n+1 before
!                  adjustment.
!          scr7  = predicted T at time step n+1 before adjustment. 
!          dum21 = L_v*L_v/(R_v*c_p)
!          dum31 = L_v
!          prc   = saturation mixing ratio at time step n+1 before adjustment.
!          prd   = c_p (function of mixing ratio)

          SCR8(I,K)=(SCR4(I,K)-PRC(I,K))/(1.+DUM21(I,K)*PRC(I,K)
     1     /(SCR7(I,K)*SCR7(I,K)))
!jmb--    SCR8 > 0 for condensation (RH stays at 100%, 
!                                    more cloud liquid water), 
!              < 0 for evaporation.

          DUM11(I,K)=SCR3(I,K)+SCR8(I,K)
          IF(DUM11(I,K).GE.0)THEN
            SCR6(I,K)=SCR8(I,K)/DT
          ELSE
cjmb--      If dum11 < 0,
!            then conditions are subsaturated AND evaporation of cloud
c            water is limited by the cloud water available (no evaporation if
c            no cloud water).      
            SCR6(I,K)=-SCR3(I,K)/DT
          ENDIF

!         Probably faster: most frequent path is drop thru
!         SCR6(I,K)=-SCR3(I,K)/DT
!         IF(DUM11(I,K).GE.0) SCR6(I,K)=SCR8(I,K)/DT 


cjmb--    Now construct MM5 tendencies with saturation adjustment results
!          included.
          QVTEN(I,K)=QVTEN(I,K)-psb(i,j)*SCR6(I,K)
C...INITIATION OF CLOUD WATER:
          QCTEN(I,K)=QCTEN(I,K)+psb(i,j)*SCR6(I,K)
          DUM21(I,K)=DUM31(I,K)/PRD(I,K)
          TTEN(I,K)=TTEN(I,K)+psb(i,j)*SCR6(I,K)*DUM21(I,K)
cmm5         TTEN(I,K)=TTEN(I,K)+PSC(I,J)*SCR6(I,K)*DUM21(I,K)*(1-IFDRY)
C--COMPUTE P*T AND P*QR (WITHOUT FALLOUT TERM) AT TAU+1:
          SCR4R(I,K)=AMAX1(R2,QRa(I,J,Kk)*psb(i,j)+DT*QRTEN(I,K))
          SCR4S(I,K)=AMAX1(R2,QNIa(I,J,Kk)*psb(i,j)+DT*QNITEN(I,K))
          SCR4G(I,K)=AMAX1(R2,QGa(I,J,Kk)*psb(i,j)+DT*QGTEN(I,K))
cjmb      scr4i(i,k)=amax1(r2,qib(i,j,kk)*psb(i,j)+dt*qiten(i,k))
          scr4i(i,k)=amax1(r2,qia(i,j,kk)*psb(i,j)+dt*qiten(i,k))
cjmb      SCR4N(i,k)=amax1(r2,qncb(i,j,kk)*psb(i,j)+dt*qncten(i,k))
          SCR4N(I,K)=max(R2,qnca(i,j,kk)*psb(i,j)+DT*QNCTEN(I,K))


cjmb--    SCR7 is set here t*psc, psc in cb!!--9feb96
cjmb      SCR7(I,K)=Tb(I,J,K)+DT*TTEN(I,K)
          scr6(i,k) = taout(i,k) + dt*tten(i,k)/psb(i,j)
   70 CONTINUE


cjmb--Loop 80 contains all fallout stuff.  This loop slgtly revised Sep 99 to 
c      conform more closely to MM5:  
c       1. No longer a separate loop for cloud ice since MRI fallout 
c          formula puts fall speeds closer to those for
c          snow.--13sep99 
c       2. Drop conditional that mixing ratios after loop 40 tendencies 
c          added must be larger than R1/psb.--13sep99 
C--COMPUTE THE FALLOUT TERMS:   (USE MINIMUM FALL SPEED OF MIXING RATIO=1.E-7)
C     COMPUTE FALL TERM WITH SHORTER TIME STEPS WHERE VFALL>DZ/DT
      R3 = 1.E-7

      DO 80 I=ist,ien
        NSTEP=0
!       ****Could phps save time here by looking at total 
!           rain+ice+snow+graupel in column and bypassing fallout if 
!           column sum of this is not > 4*(MKX+1)*R1.--15feb00
cjmb--  Save accumulated pcpn at this particular i,j point at start of 
c        microphysics time step in order to compute the
c        average pcpn rate during step.--8apr97 
        pcpnrb = pcpnr(i,j)
        pcpnib = pcpni(i,j)
        pcpnsb = pcpns(i,j) 
        pcpngb = pcpng(i,j)
 
cjmb--  Loop 90 sets up vertical arrays needed for fallout calculation.
c        All these arrays are defined on MAPS computational levels.
!       Need to revisit use of density in this loop.  I think it doesnt
!        really matter in practice, but there is room for some slight
!        inconsistency in treatment of downward flux of 
!        hydrometeors--15feb00.**** 
        opsb = 1./psb(i,j)

        DO 90 K=1,KL
          kk=kl-k+1
          VT2R = 0.
          VT2S = 0.
          VT2G = 0.
          VT2I = 0.

cjmb--    RHOD from loop 15 is in SI units, whereas PSC is in cb, the MM5 unit.
c          This density
c          is needed in expressions for lambda_x used to generate fall speeds
c          based on time-step n+1 mixing ratios of falling hydrometeors.  Note 
c          that SCR4R, SCR4S, SCR4G , SCR4I are really PSB*mixing ratio.
!          Therefore,
c          divide rho by psb to get units to come out right.
          pd=.62198*pres(i,k)/((qva(i,j,kk) + qvten(i,k)*opsb)+.62198)  
          rho3=(pd/(r*scr6(i,k))) ! Units kg/m^3 (SI)

cjmb--    Could speed up the fall speed calculation by precalculating
c          intercepts and slopes for R3 in PARAMR.  Also, could use
c          intercept values from loop 20.  For snow, use of a constant
c          fall speed of 1m/s (or fall speed of 1m/s for TAOUT < TO and 
c          fall speed increasing to 3m/s as TAOUT increases to 5C)
c          is also a viable option.--15dec00
!jmb--For rain
!         IF (SCR4R(I,K).GT.R1) THEN ! commented 13sep99
                                                 
          updt_r=max(r3,scr4r(i,k)*opsb)
          IF (UPDT_R.GT. R3) THEN
Cgt         ronv(i,k) = const1r*tanh((qr0 - rain(i,k))/delqr0) + const2r       ! BUG
            ronv(i,k) = const1r*tanh((qr0 - updt_r)/delqr0) + const2r       
          ELSE
Cgt         RONV(I,K) = RON2       ! Bug
            RONV(I,K) = RON        ! GREG T.  01 Dec 2004
          ENDIF
          RONV(I,K) = RONV(I,K)/RON
          SLOR1=(RHO3*UPDT_R/(TOPR*RONV(I,K)))**0.25
          VT2R=(FRAIN*SLOR1**BR)*RHO_FAC(I,K)
csgbjmb
c   1/28/97 - limit rain fall speed to 5 m/s   WHY???
Cgt       VT2R=min (5.0,vt2r)
          VT2R=min (9.0,vt2r)
c         ENDIF
 
!jmb--For snow
!         IF (SCR4S(I,K).GT.R1) THEN ! commented 13sep99

!jmb--    Choice here:  1) Recalculate normalized snow intercept on basis of 
!                          new snow mixing ratio predicted previous loops
!                          this call to EXMOISG (pre-1998 and Y2K versions).
!                       2) Use old normalized snow intercept calculated loop 20 

          updt_s=max(r3,scr4s(i,k)*opsb)
          IF (UPDT_S .GT. R3) THEN
Cgt         rhoqs=rho3*updt_s
Cgt         sonv(i,k)=const_ns1*(rhoqs**const_ns2)
Cgt         SONV(I,K) = MIN(SONV(I,K),SON)
C.. New SONV formulation based on Fig. 7, curve_3 of Houze et al 1979   ! GREG T.  
            temp_C = amin1(-0.001, SCR6(I,K)-273.15)                    ! GREG T.  
            sonv(i,k) = amin1(2.0E8, 2.0E6*exp(-0.12*temp_C))           ! GREG T.
          ELSE
            SONV(I,K) = SON
          ENDIF
          SONV(I,K) = SONV(I,K)/SON
          SLOS1=(RHO3*UPDT_S/(TOPS*SONV(I,K)))**0.25
          VT2S =(FSNOW*SLOS1**BS)*rho_fac(i,k)
          VT2S = min(2.0, VT2S)                                         ! GREG T.
c         ENDIF

!jmb--For graupel
!c        IF (SCR4G(I,K).GT.R1) THEN

!jmb--    Choice here:  1) Recalculate normalized graupel intercept on basis of 
!                          new graupel mixing ratio predicted previous loops
!                          this call to EXMOISG (pre-1998 and Y2K versions).
!                       2) Use old normalized graupel intercept 
!                          calculated loop 20 
          UPDT_G = AMAX1(R3,SCR4G(I,K)*oPSb)
          IF (UPDT_G .GT. R3) THEN   
            rhoqg=rho3*updt_g
            gonv(i,k) = const_ng1*(rhoqg**const_ng2)
            GONV(I,K) = MAX(1.E4,MIN(GONV(I,K),GON))
          ELSE
            GONV(I,K) = GON
          ENDIF
          GONV(I,K) = GONV(I,K)/GON
          SLOG1=(RHO3*UPDT_G/(TOPG*GONV(I,K)))**0.25 
          VT2G=(FGRAUPEL*SLOG1**BG)*rho_fac(i,k)
          VT2G = min(10.0, VT2G)                                        ! GREG T.
cc        ENDIF

cjmb--    For settling of ice crystals: Use either:
c            1) Heymsfield and Donner (1990, JAS, Eq 5)
c            2) MRI (preferred by Roy R.)
cjmb--    Units: m/s. Assume that QNC falls at same rate.--10jun97 

          UPDT_I = AMAX1(R3,SCR4I(I,K)*oPSb)
          UPDT_N = AMAX1(R3,SCR4N(I,K)*oPSb)
cc        vt2i=3.29*(rho3*updt_i(i,k))**0.16  ! Heymsfield and Donner
          DIAMI=(6.*RHO3*UPDT_I/(PI*DICE*UPDT_N))**0.3333
          DIAMI=AMAX1(AMIN1(2.*XR0S, DIAMI), DIACE_min)                 ! GREG T.
Cgt       IF (DIAMI .GT. 2.*XR0S) DIAMI = 2*XR0S 
          VT2I=700.*DIAMI*RHO_FAC(i,k)                                  ! GREG T.
          VT2I=AMAX1(AMIN1(1.0, VT2I), 0.3)                             ! GREG T.
Cgt       DIAMI=AMAX1(DIACE_min, (6.*RHO3*UPDT_I                ! GREG T.
Cgt  +          /(PI*DICE*UPDT_N))**0.3333)                     ! GREG T.
Cgt       VT2I=700. * AMIN1(DIAMI, 2.*XR0S) * RHO_FAC(i,k)      ! GREG T.

C UNITS ARE G-M/S2, RHO3-KG/M3,VT2-M/S,PSB-CB*1000=PA
!          which makes F[RSGI] s{-1} = [sigma/second]
cjmb--    convert fall speeds m/s ---> units of sigma, where
cjmb       sigma = pres/psc.  

cjmb--    ***Since RHO3 is dry-air density in SI units, and it is divided 
!          by psb, which is
cjmb       in centibars, the factor of .001 must multiply to put psb in pascals.

          rho3g=g*rho3*0.001*opsb
          fr(k)=rho3g*vt2r
          fi(k)=rho3g*vt2i
          fs(k)=rho3g*vt2s
          fg(k)=rho3g*vt2g
          fnc(k)=rho3g*vt2i


   90   continue

cjmb--  dsigma for MAPS application (no vertical staggering).  
c        DSIGMA in MM5 is brought in via common
c        block.  DSIGMA calculated here in MM5 format (upside down).  
c        Multiply by .001 to convert psc to pascals from centibars.
c        Use pressure thickness upstream of computational level for thickness
c        to accomodate upstream-forward space differencing (see comment below). 

cjmb    Arbitrarily assign 
        dsigma(1) = .05 ! Equivalent to about 50mb
        do 92 k=2,kl
          dsigma(k) = .001*(pres(i,k) - pres(i,k-1))*opsb 
   92   continue


cjmb--  Loop 94 calculates the fastest fall speed for any
cjmb     specie in order to ensure that the fallout calculation uses a
cjmb     sufficiently sml time step--11may96

        do 94 k=1,kl
          RGVM=MAX(FR(K),FS(K),FG(K),FI(K))

c 1 is to round up, represents number of steps
          NSTEP=MAX0(IFIX(RGVM*DT/DSIGMA(K)+1.),NSTEP)
c         NSTEP=50

!jmb      Could use to have longer time step for ice
cc        nstepi=max0(ifix(fi(k)*dt/dsigma(k)+1.),nstepi) 

   94   continue
 

cjmb--  Loop 100--actual fallout calculation, done in NSTEP subdivisions of 
cjmb     time interval between calls to EXMOISG.
cjmb--  Expression for tendency of pstar*q_x used here, 
cjmb      where q_x is mixing ratio of rain, snow or graupel, is
cjmb
cjmb
cjmb                               partial [(q_x*pstar)*sigmadot_x)]
cjmb     Tendency of pstar*q_x = - --------------------------------.
cjmb       due to fallout          partial sigma
cjmb
cjmb    
cjmb     This is derived from the fundamental expression, derived from the
cjmb     continuity equations for falling water substance
cjmb
cjmb                                1     partial (rho*q_x*w_Tx)
cjmb     Mixing-ratio tendency = + ---*   ----------------------,
cjmb       due to fallout          rho    partial z
cjmb
cjmb     where rho is air density 
cjmb     (ignoring small difference between dry air and
cjmb     moist air density) and w_Tx (> 0) is downward mass-weighted  
cjmb     fall speed (m/s) for x.
cjmb   
cjmb     In actual application, a forward-upstream flux calculation is 
cjmb     imposed.  This accounts for the use of the fallout fluxes at level
cjmb     k and level k-1 (in MM5 upside-down style) for prediction of 
cjmb     tendency at level k.  

 
        DO 100 N=1,NSTEP
          DO 110 K=1,KL

            FALOUTR(K)=FR(K)*SCR4R(I,K) ! kg/kg s{-1}
            FALOUTS(K)=FS(K)*SCR4S(I,K)
            FALOUTG(K)=FG(K)*SCR4G(I,K)
            FALOUTI(K)=FI(K)*SCR4I(I,K)
            FALOUTN(K)=FI(K)*SCR4N(I,K)

  110     CONTINUE

C FOR TOP OF MODEL
          K=1
          FALTNDR=FALOUTR(K)/DSIGMA(K)
          FALTNDS=FALOUTS(K)/DSIGMA(K)
          FALTNDG=FALOUTG(K)/DSIGMA(K)
          FALTNDI=FALOUTI(K)/DSIGMA(K)
          FALTNDN=FALOUTN(K)/DSIGMA(K)

          QRTEN(I,K)=QRTEN(I,K)-FALTNDR/NSTEP
          QNITEN(I,K)=QNITEN(I,K)-FALTNDS/NSTEP 
          QGTEN(I,K)=QGTEN(I,K)-FALTNDG/NSTEP     
          QITEN(I,K)=QITEN(I,K)-FALTNDI/NSTEP
          QNCTEN(I,K)=QNCTEN(I,K)-FALTNDN/NSTEP
 

          SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP
          SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP
          SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP
          SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP
          SCR4N(I,K)=SCR4N(I,K)-FALTNDN*DT/NSTEP

          DO 120 K=2,KL

            FALTNDR=(FALOUTR(K)-FALOUTR(K-1))/DSIGMA(K)
            FALTNDS=(FALOUTS(K)-FALOUTS(K-1))/DSIGMA(K)
            FALTNDG=(FALOUTG(K)-FALOUTG(K-1))/DSIGMA(K)
            FALTNDI=(FALOUTI(K)-FALOUTI(K-1))/DSIGMA(K)
            FALTNDN=(FALOUTN(K)-FALOUTN(K-1))/DSIGMA(K)

            QRTEN(I,K)=QRTEN(I,K)-FALTNDR/NSTEP
            QNITEN(I,K)=QNITEN(I,K)-FALTNDS/NSTEP
            QGTEN(I,K)=QGTEN(I,K)-FALTNDG/NSTEP
            QITEN(I,K)=QITEN(I,K)-FALTNDI/NSTEP
            QNCTEN(I,K)=QNCTEN(I,K)-FALTNDN/NSTEP

            SCR4R(I,K)=SCR4R(I,K)-FALTNDR*DT/NSTEP
            SCR4S(I,K)=SCR4S(I,K)-FALTNDS*DT/NSTEP
            SCR4G(I,K)=SCR4G(I,K)-FALTNDG*DT/NSTEP
            SCR4I(I,K)=SCR4I(I,K)-FALTNDI*DT/NSTEP
            SCR4N(I,K)=SCR4N(I,K)-FALTNDN*DT/NSTEP


  120     CONTINUE

C     ACCUMULATED RAIN
cjmb--    Desired precipitation accumulation is in cm. 
cjmb       The basis for the calculation is as follows.
cjmb       Downward flux of hydrometeor x toward the ground relative 
cjmb       to air motion is  
cjmb
cjmb   (dry air density) * (mixing ratio of x) * (mass weighted terminal
cjmb                                                speed of x)
cjmb       Units: ML^-3 * 0 * L/T = M L^-2 T^-1.
cjmb       In SI units this translates to kg*m**-2*s**-1, or, for melted 
cjmb       equivalent using a density of 1000 kg*m**-3, mm/s.
cjmb
cjmb      This flux is, in terms of sigma-dot for fall speed, approximately
cjmb   
cjmb                      (q_x*pstar)*sigmadot_x/g,
cjmb
cjmb       where pstar is in pascals (not centibars).
cjmb 
cjmb       The pcpn rate (liquid equivalent, mm/s) for x is then 
cjmb
cjmb             faloutx*1000./g, 
cjmb
cjmb       where x is either r, s or g, for rain, snow or graupel,
cjmb       and faloutx is FALOUTR, FALOUTS or FALOUTG.
cjmb       The factor 1000 comes in because the local variable scr4x
cjmb       used in faloutx, where x is either r,s or g, is in MM5 format,
cjmb       i.e., is q_x times PSC, where PSC is in cb, and has to be
cjmb        multiplied by 1000 to convert to pascals.
cjmb 
cjmb      To convert to accumulation in cm during each fallout time step, 
cjmb       the factor to multiply faloutx is then
cjmb
cjmb   0.1 [mm-->cm] * 1000. [centibars-->pascals] * (dt/nstep) / g [m/s**-2]
cjmb
cjmb       This is equivalent to the factor used in the MM5 formulation.
cjmb       The constant portion of this factor, 0.1*1000./g,
cjmb       is precalculated as DTPCPN in INITHYBV.

cjmb--    Accumulated precipitation as rain, snow and graupel should
cjmb       be kept track of separately as well as together.
cjmb   

          pcpnr(i,j)=pcpnr(i,j)+faloutr(kl)*dtrain*dtpcpn/float(nstep) 
          pcpni(i,j)=pcpni(i,j)+falouti(kl)*dtrain*dtpcpn/float(nstep) 
          pcpns(i,j)=pcpns(i,j)+falouts(kl)*dtrain*dtpcpn/float(nstep) 
          pcpng(i,j)=pcpng(i,j)+faloutg(kl)*dtrain*dtpcpn/float(nstep) 

          RAINNC(I,J)=RAINNC(I,J) + (faloutr(kl) + falouts(kl) +
     1            faloutg(kl)+falouti(kl))*dtrain*dtpcpn/float(nstep)

          pcpnc1h(I,J)=pcpnc1h(I,J) + (faloutr(kl) + falouts(kl) +
     1        faloutg(kl)+falouti(kl))*dtrain*dtpcpn/float(nstep)*10.
!         pcpnc1h in mm, not cm--this is why the factor of 10.

  100   CONTINUE

cjmb--  Average precipitation rates during microphysics time step--8apr97
cjmb--  PRECINR, PRECINI, PRECINS, PRECING declared and dimensioned in POINT2D.
cjmb--For rain: cm/h
        precinr(i,j) = (pcpnr(i,j) - pcpnrb)*3600./dt 
cjmb--For ice crystals, graupel and snow: SOIL123 uses rates in m/s
        precini(i,j) = (pcpni(i,j) - pcpnib)*.01/dt
        precins(i,j) = (pcpns(i,j) - pcpnsb)*.01/dt 
        precing(i,j) = (pcpng(i,j) - pcpngb)*.01/dt
cjmb--Grid-scale pcpn rate in cm/h (3600*100 m/s = cm/h).
        grdpcpn(i,j) = precinr(i,j) + 
     1    (precini(i,j) + precins(i,j) + precing(i,j))*360000.

   80 CONTINUE

cjmb--Limited changes to Loop 95 to account for MAPS-MM5 differences in
c      variable definitions (mainly QCB, QIB).  

      IF(IIWARM)GOTO 705
      DO 135  I=ist,ien
        DO 130 K=2,KL

cjmb--    Apparently it is assumed that temperature at highest model level is
cjmb       < HGFR=233 K.
          kk=kl-k+1
cjmb--    Flip indices since some MAPS variables used.
cjmb       K is MM5; kk, MAPS--20jul96

C MELTING OF CLOUD ICE
!jmb--    Note that no account is taken of the possibility that melting of
!          cloud ice may bring the temperature back to freezing or below,
!          or that the cooling by melting may cause relative humidity > 100%.
!          Another saturation-adjustment iteration would be necessary to avoid
!          these possibilities.

          IF ( (SCR6(I,K).GT.TO) .AND. ( (QITEN(I,K).GT.0.) .OR.
cjmb 1     (QIB(I,J,K).GT.0.) ) ) THEN
     1     (QIa(I,J,kk).GT.0.) ) ) THEN

!         Then temp at time ktau + 1 is > 0 deg C so melt any ice. 

            XLATF=XLS-DUM31(I,K)
cjmb        DUM11(I,K)=QIB(I,J,K)+DT*QITEN(I,K)

!jmb        Need to ensure that dum11 is >0, otherwise will melt negative ice!!
!            That is to say, unless dum11 > 0, no need to melt, since q_i will
!            be zero anyway.
            dum11(i,k)=max(0.,(qia(i,j,kk) * psb(i,j) + dt*qiten(i,k)))
Cgt         QITEN(I,K)= qiten(i,k) - dum11(i,k)/dt  ! Dec 03
            QITEN(I,K)= qiten(i,k) - dum11(i,k)/dt 
cjmb        QITEN(I,K)= -qia(i,j,kk)*psb(i,j)/dt ! 30 Dec 04
            QCTEN(I,K)=QCTEN(I,K)+DUM11(I,K)/DT
            QNCTEN(I,K)=-QNCa(I,J,kk)*psb(i,j)/DT
            TTEN(I,K)=TTEN(I,K)-XLATF/PRD(I,K)*DUM11(I,K)/DT
cmm5           TTEN(I,K)=TTEN(I,K)-XLATF/PRD(I,K)*DUM11(I,K)/DT
cmm5 +              *(1-IFDRY)
          ENDIF

C HOMOGENIOUS FREEZING OF CLOUD ICE [R: (A.23)]
          IF( (SCR6(I,K).LT.HGFR) .AND. ( (QCTEN(I,K).GT.0.) .OR.
cjmb 1     QCB(I,J,K).GT.0.)) THEN
     1     (QCa(I,J,kk).GT.0.) ) ) THEN
            XLATF=XLS-DUM31(I,K)
!      Need to ensure that dum11 is >0, otherwise may freeze negative 
!      cloud water.
            DUM11(I,K)=max(0.,(QCa(I,J,kk) * psb(i,j) + DT*QCTEN(I,K)))
Cgt         qcten(i,k) = qcten(i,k) - dum11(i,k)/dt ! Dec 03
            qcten(i,k) = qcten(i,k) - dum11(i,k)/dt
Cjmb        qcten(i,k) = -qca(i,j,kk) * psb(i,j)/dt ! 30 Dec 04
            QITEN(I,K)=QITEN(I,K)+DUM11(I,K)/DT
Cgt         QNCTEN(I,K)=QNCTEN(I,K)+(RHOd(i,k)*DUM11(I,K)/XM01)/DT
Cgt  1              *PSB(I,J)
            QNCTEN(I,K)=QNCTEN(I,K)+(DUM11(I,K)/XM01)/DT      ! GREG T.  BUG FIX

            TTEN(I,K)=TTEN(I,K)+XLATF/PRD(I,K)*DUM11(I,K)/DT
cmm5           TTEN(I,K)=TTEN(I,K)+XLATF/PRD(I,K)*DUM11(I,K)/DT
cmm5 +              *(1-IFDRY)

          ENDIF
  130   continue
  135 continue

  705 continue  ! Jump to here if IIWARM is true.

cjmb--Limit latent heating for TTEN and save temp tendencies for GCIP. 
      DO K=1,KL
        kk=kl-k+1
        DO I=ist,ien
        opsb = 1./psb(i,j)
          exiqvten(i,j,k) = (qvten(i,k) - qvtenpre(i,k))*opsb
          exitten(i,j,k) = (tten(i,k)-ttenpre(i,k))*opsb
          exitten(i,j,k)=min(exitten(i,j,k),0.1*opsb)
          exitten(i,j,k)=max(exitten(i,j,k),-0.1*opsb)
          exiqcten(i,j,k)  = qcten(i,k)  * opsb
          exiqiten(i,j,k)  = qiten(i,k)  * opsb
          exiqrten(i,j,k)  = qrten(i,k)  * opsb
          exiqniten(i,j,k) = qniten(i,k) * opsb
          exiqgten(i,j,k)  = qgten(i,k)  * opsb
          exiqncten(i,j,k) = qncten(i,k) * opsb

csgb -- units of these tendencies are hundredths of degrees (K) /s
c    --   i.e.  2.e-1 = 0.002 K / s, since tten = dT/dt * sfc pressure in cb.
          if (facility_p .eq. fsl_p) then
             DTGP(I,J,KK)=(tten(i,k) - ttenpre(i,k))*opsb
          endif

        enddo
      enddo


      RETURN
      END

