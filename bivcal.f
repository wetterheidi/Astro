!*****************************************************************
!     BERECHNUNG NACHTHELLIGKEIT                                        
!     SUBROUTINE BIVCAL                                                 
!                                                                       
!     VERSION 98.1 SEIT 19.02.1998                                      
!     VERSION 98.2 SEIT 03.06.1998                                      
!     VERSION 98.21 SEIT 19.10.1998                                     
!             TESTAUSDRUCK BESEITIGT, DER DAZU FüHRTE, DAß BEI VN=0KM   
!             FüR M IMMMER VN=3.0KM GESETZT WURDE.                      
!     VERSION 01.1 SEIT 17.05.2001                                      
!             PLTTIM GENAUER BESTIMMT UM RUNDUNGSFEHLER BEIM            
!             RECHNEN MIT EINEM ZEITSCHRIT VON 1 MIN ZU VERMEIDEN       
!     VERSION 09.7 27.07.09 SCHMITT
!             NEUE EPHEMERIDEN 2001-2030
!                                                                       
!*****************************************************************      
      SUBROUTINE BIVCAL(IS_WEB_APPL)                                               
           
      USE BIV_UTILS           
      INCLUDE 'bivcom.h'

      LOGICAL, INTENT(IN) :: IS_WEB_APPL      
                                                                        
      INTEGER*4 I,I2                                                
      INTEGER*4 E1,W1,N1                                                
      INTEGER*4 I1                                                      
      INTEGER*4 GRAD,MINU,IART                                          
      INTEGER*4 YER,MON,DAY,HOR,MNT                                     
                                                                        
      REAL*4 RDUM,KORR,BEDMO                                            
      REAL*4 R1,R2                                                      
      REAL*4 SHOEH,MHOEH,ELONG,HELLIG,GBS,GBM,GRND                      
      REAL*4 BEDECK,MPHASE,MHOPA                                        
      REAL*4 SONREK,MONREK,SONDEK,MONDEK,ENTME,ENTSE                    
                                                                        
      REAL*8 HOEHE,JUL,STRTIM,ENDTIM,PLTTIM                                                
      REAL*8 TVM1,TVM2,TNM1,TNM2,TAKT,JSTEP,JSTEP2
                                                                        
      CHARACTER*11 C_BREITE,C_LAENGE                                    
      CHARACTER*12 C_DATUM                                              
                                                                        
!     INITIALISIEREN                                                    
      HOEHE  = 0.D000          
      HELDAT = -9999.      
      TAG1   = -999                                                    
      TAG2   = -999
      ABD    = -999
      EBD    = -99
      MOON   = -9999. 
      
!     Speichern von Mimimum-Werten fuer Beleuchtungsstaerke und Sichtweite
      DATENR(:,9)  = HUGE(0.0)
      DATENR(:,10) = HUGE(0.0)
      DATENR(:,11) = HUGE(0.0)
      DATENR(:,12) = HUGE(0.0)
                                                                                                                                                
!     EINLESEN DER BLENDWIRKUNG AUS DATEI                               
!     1. INDX= MONDHOEHE (GRAD)                                         
!     2. INDX= MONHELLIGKEIT (%)                                        
      DO I1=1,60                                                        
        READ (FU_BLDFIL,FMT='(60(I1))') (BLEND(I1,I2),I2=1,60)                 
      ENDDO                                                             
      CLOSE(FU_BLDFIL)                                                        
                                                                        
!     ERZEUGEN VON KONTROLLAUSDRUCK                                     
      IF (OUTPUT) THEN                              
        WRITE (FU_OUTFIL,
     &  FMT='("ERGEBNISSE BIVPROG-LAUF FASSUNG 08/97 ")')
      ENDIF
                                                                        
!     UMWANDLUNG DER GEOGR. KOORDINATEN IN C11-STRINGS                  
      C_BREITE='000_00_00_N'                                            
      C_LAENGE='000_00_00_E'                                            
      CALL GDZGMS(GB,GRAD,MINU,SEC,C_BREITE)                            
      CALL GDZGMS(GL,GRAD,MINU,SEC,C_LAENGE)                            
                                                                        
!     UMWANDLUNG VON DATUM IN C12-STRING                                
      C_DATUM='000000000000'                                          
                                                                        
!     AKTUELLES DATUM 00Z                                               
      WRITE(C_DATUM(1:4),'(I4.4)') DATTIM(3)                            
      WRITE(C_DATUM(5:6),'(I2.2)') DATTIM(2)                            
      WRITE(C_DATUM(7:8),'(I2.2)') DATTIM(1)                            
      CALL DATDAY(DATTIM(3),DATTIM(2),DATTIM(1),0,0,TAKT)    
                                                                        
!     BESTIMMUNG DER VOLLMONDE VOR UND NACH DEM AKTUELLEN DATUM         
                                                                        
      CALL MONPHA(C_DATUM,TVM1,TNM1,TZUMON,TABMON)                      
      JUL=TAKT                                                          
      DO WHILE(TVM1.GT.TAKT)                                            
        JUL=JUL-1.                                                      
        CALL DAYDAT(JUL,YER,MON,DAY,HOR,MNT)                            
        WRITE(C_DATUM(1:4),'(I4.4)') YER                                
        WRITE(C_DATUM(5:6),'(I2.2)') MON                                
        WRITE(C_DATUM(7:8),'(I2.2)') DAY                                
        CALL MONPHA(C_DATUM,TVM1,TNM1,TZUMON,TABMON)                    
      ENDDO                                                             
                                                                        
      CALL MONPHA(C_DATUM,TVM2,TNM2,TZUMON,TABMON)                      
      JUL=TAKT                                                          
      DO WHILE(TAKT.GT.TVM2)                                            
        JUL=JUL+1.                                                      
        CALL DAYDAT(JUL,YER,MON,DAY,HOR,MNT)                            
        WRITE(C_DATUM(1:4),'(I4.4)') YER                                
        WRITE(C_DATUM(5:6),'(I2.2)') MON                                
        WRITE(C_DATUM(7:8),'(I2.2)') DAY                                
        CALL MONPHA(C_DATUM,TVM2,TNM2,TZUMON,TABMON)                    
      ENDDO                                                             
                                                                        
!     AUF AKTUELLES DATUM 00Z ZURUECKSETZEN                             
      WRITE(C_DATUM(1:4),'(I4.4)') DATTIM(3)                            
      WRITE(C_DATUM(5:6),'(I2.2)') DATTIM(2)                            
      WRITE(C_DATUM(7:8),'(I2.2)') DATTIM(1)                            
                                                                        
!     BERECHNUNG DER NAECHSTGELEGENEN MONDPHASEN                        
      CALL MONPHA(C_DATUM,TVOLMO,TNEUMO,TZUMON,TABMON)                  
                                                                        
!************ A N F A N G  F O L G E T A G ************************     
                                                                        
!     MONDAUFRUF FUER FOLGETAG (GLEICHZEITIG RUECKGABE VON SONNENDATEN) 
!     C_DATUM UM 1 ERHOEHEN                                             
                                                                        
      READ(C_DATUM(1:4),'(I4)') YER                                     
      READ(C_DATUM(5:6),'(I2)') MON                                     
      READ(C_DATUM(7:8),'(I2)') DAY                                           
      CALL DATDAY(YER,MON,DAY,0,0,JUL)                                              
      JUL=JUL+1                                                                    
      CALL DAYDAT(JUL,YER,MON,DAY,HOR,MNT)                              
      WRITE(C_DATUM(1:4),'(I4.4)') YER                                  
      WRITE(C_DATUM(5:6),'(I2.2)') MON                                  
      WRITE(C_DATUM(7:8),'(I2.2)') DAY
      FOLGDATE = C_DATUM(7:8)//'.'//C_DATUM(5:6)//'.'
                                                                        
      CALL UPMOND (C_BREITE,C_LAENGE,C_DATUM,HOEHE,LISTEM,LISTES,1,     
     %             MAW,MUW,TVM1,TVM2,TNM1,TNM2)                                                                                                 
      HMON2=LISTEM(9)
                                                                        
!     MONDAUF/UNTERGANG, SONNENAUF/UNTERGANG, EDN,ADN,ABD,EBD           
!     TAG 1EDN 2AND 3ABD 4SA 5SU                                        
!     6EBD 7END 8ADN 9MA 10MU  11MALTER 12NEUVOL                        
                                                                        
!     ENDE DER DUNKLEN NACHT            (EDN =1)                        
      RDUM = FGMSAK(SNGL(LISTES(11)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(1) = NINT(RDUM*100)                              
                                                                        
!     ANFANG DER NAUTISCHEN DAEMMERUNG  (AND =2)                        
      RDUM = FGMSAK(SNGL(LISTES(12)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(2) = NINT(RDUM*100)                              
                                                                        
!     ANFANG DER BUERGERL. DAEMMERUNG   (ABD =3)                        
      RDUM = FGMSAK(SNGL(LISTES(18)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) THEN
         TAG2(3) = NINT(RDUM*100)           
      ENDIF
                                                                        
!     SONNENAUFGANG                     (SA =4)                         
      RDUM = FGMSAK(SNGL(LISTES(13)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(4) = NINT(RDUM*100)                              
                                                                        
!     SONNENUNTERGANG                   (SU =5)                         
      RDUM = FGMSAK(SNGL(LISTES(14)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(5) = NINT(RDUM*100)                              
                                                                        
!     ENDE DER BUERGERL. DAEMMERUNG     (EBD =6)                        
!      RDUM = FGMSAK(SNGL(LISTES(17)))                                   
!      IF (RDUM.GT.-999.AND.RDUM.LT.999) THEN                                 
!         TAG2(6) = NINT(RDUM*100)                              
!      ENDIF
                                                                        
!     ENDE DER NAUTISCHEN DAEMMERUNG    (END =7)                        
      RDUM = FGMSAK(SNGL(LISTES(15)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(7) = NINT(RDUM*100)                              
                                                                        
!     ANFANG DER DUNKLEN NACHT          (ADN =8)                        
      RDUM = FGMSAK(SNGL(LISTES(16)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(8) = NINT(RDUM*100)                              
                                                                        
!     MONDAUFGANG                       (MA =9)                         
      RDUM = FGMSAK(SNGL(LISTEM(13)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &            TAG2(9) = NINT(RDUM*100)                              
                                                                        
!     MONDUNTERGANG                     (MU =10)                        
      RDUM = FGMSAK(SNGL(LISTEM(14)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999)                                 
     &           TAG2(10) = NINT(RDUM*100)                              
                                                                        
!     TAGE VOR(-) NACH(+) VOLL-/NEUMOND                                 
      TAG2(11)=INT(LISTEM(17))                                          
                                                                        
!     NEU-(1) / VOLLMOND(2)                                             
      TAG2(12)=INT(LISTEM(18))                                          
                                                                        
!************ E N D E   F O L G E T A G  **************************     
                                                                        
!************ A N F A N G         T A G  **************************     
                                                                                                                                                
!     AUF AKTUELLES DATUM 00Z ZURUECKSETZEN                             
      WRITE(C_DATUM(1:4),'(I4.4)') DATTIM(3)                            
      WRITE(C_DATUM(5:6),'(I2.2)') DATTIM(2)                            
      WRITE(C_DATUM(7:8),'(I2.2)') DATTIM(1)                            
      ACTDATE = C_DATUM(7:8)//'.'//C_DATUM(5:6)//'.'  
                                                                        
      CALL UPMOND (C_BREITE,C_LAENGE,C_DATUM,HOEHE,LISTEM,LISTES,1,     
     %             MAW,MUW,TVM1,TVM2,TNM1,TNM2)                         
                                                                        
      HMON1=LISTEM(9)                                                   
            
!     Entfernung Erde-Mond      
      ENTF_MOON_EARTH=SNGL(LISTEM(7))

!     ENDE DER BUERGERL. DAEMMERUNG     (EBD =6)                        
      RDUM = FGMSAK(SNGL(LISTES(17)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) THEN                                 
         TAG2(6) = NINT(RDUM*100)                              
      ENDIF
                                                                                   
!     MONDAUF/UNTERGANG, SONNENAUF/UNTERGANG, EDN,ADN,ABD,EBD           
!     ENDE DER DUNKLEN NACHT            (EDN =1)                        
      RDUM = FGMSAK(SNGL(LISTES(11)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(1) = NINT(RDUM*100)        
                                                                        
!     ANFANG DER NAUTISCHEN DAEMMERUNG  (AND =2)                        
      RDUM = FGMSAK(SNGL(LISTES(12)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(2) = NINT(RDUM*100)        
                                                                        
!     ANFANG DER BUERGERL.  DAEMMERUNG  (ABD =3)                        
      RDUM = FGMSAK(SNGL(LISTES(18)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(3) = NINT(RDUM*100)        
                                                                        
!     SONNENAUFGANG                     (SA =4)                         
      RDUM = FGMSAK(SNGL(LISTES(13)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(4) = NINT(RDUM*100)        
                                                                        
!     SONNENUNTERGANG                   (SU =5)                         
      RDUM = FGMSAK(SNGL(LISTES(14)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(5) = NINT(RDUM*100)        
                                                                        
!     ENDE DER BUERGERL. DAEMMERUNG     (EBD =6)                        
      RDUM = FGMSAK(SNGL(LISTES(17)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(6) = NINT(RDUM*100)        
                                                                        
!     ENDE DER NAUTISCHEN DAEMMERUNG    (END =7)                        
      RDUM = FGMSAK(SNGL(LISTES(15)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(7) = NINT(RDUM*100)        
                                                                        
!     ANFANG DER DUNKLEN NACHT          (ADN =8)                        
      RDUM = FGMSAK(SNGL(LISTES(16)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(8) = NINT(RDUM*100)        
                                                                        
!     MONDAUFGANG                       (MA =9)                         
      RDUM = FGMSAK(SNGL(LISTEM(13)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(9) = NINT(RDUM*100)        
                                                                        
!     MONDUNTERGANG                     (MU =10)                        
      RDUM = FGMSAK(SNGL(LISTEM(14)))                                   
      IF (RDUM.GT.-999.AND.RDUM.LT.999) TAG1(10) = NINT(RDUM*100)       
                                                                        
!     MOND-ELONGATION                                                   
      ELONG = SNGL(LISTEM(15))                                          
                                                                        
!     TAGE VOR(-) /BZW NACH(+) VOLLMOND                                 
      TAG1(11)=INT(LISTEM(17))                                          
                                                                        
!     NEUMOND=1 VOLLMOND=2                                              
      TAG1(12)=INT(LISTEM(18))                                          
!                                                                       
      IF(OUTPUT) THEN                           
        WRITE (FU_OUTFIL,'(A)') 'DAEMMERUNG: '//                               
     %   ' EDN  AND  ABD   SA   SU  EBD  END  ADN   MA   MU TAGE  N/V'  
        WRITE (FU_OUTFIL,FMT='(12X,12(I4,1X))') (TAG1(I2),I2=1,12)             
      ENDIF                                                             
!************ E N D E             T A G  **************************     
                                                                        
!############   BIV BERECHNUNG #####################################    
      
      IF (.NOT.IS_WEB_APPL) THEN   
        OPEN(FU_HELDAT,FILE=FNAME_HELDAT)
        OPEN(FU_MOONDAT,FILE=FNAME_MOONDAT)
      ENDIF 
      STRTIM = START_TIMES(1)
      ENDTIM = END_TIMES(ANZ)
      IF (STRTIM > ENDTIM) THEN
         IF (IS_WEB_APPL) THEN
            CALL EXIT_WEB_ERROR('Starttermin >= Endtermin!',.false.) 
          ELSE
            PRINT *,'Starttermin >= Endtermin: ',STRTIM,ENDTIM
            CALL EXIT(-1)
          ENDIF      
      ENDIF
      
!     PLOT-STEP berechnen
      CALL DATDAY(DATTIM(3),DATTIM(2),DATTIM(1),0,0,JUL)
      CALL DATDAY(DATTIM(3),DATTIM(2),DATTIM(1),0,NINT(PSTEP),JSTEP)
      JSTEP = JSTEP - JUL
      JSTEP2 = JSTEP / 2.0
      ENDTIM = ENDTIM - JSTEP2
      
      PLTTIM=STRTIM                                                     
      I=1                                                               
      IOLD=0                                                            
      IHELDA=0 
      
      DO WHILE (PLTTIM<ENDTIM.AND.IHELDA.LT.999)          
        IHELDA=IHELDA+1                                                 

        CALL DAYDAT(PLTTIM,YER,MON,DAY,HOR,MNT)      
        WRITE(C_DATUM(1:4),'(I4.4)')  YER                                
        WRITE(C_DATUM(5:6),'(I2.2)')  MON                                
        WRITE(C_DATUM(7:8),'(I2.2)')  DAY                                
        WRITE(C_DATUM(9:10),'(I2.2)') HOR
        WRITE(C_DATUM(11:12),'(I2.2)')MNT
                                                                                
        IF(OUTPUT) THEN                         
          WRITE(FU_OUTFIL,'(A)')
     &    '-------------------------------------------' 
          WRITE (FU_OUTFIL,
     &    FMT='("BREITE,LAENGE ",A11," ",A11," DATUM: ",A14)')
     &        C_BREITE,C_LAENGE,C_DATUM                    
        ENDIF                                                           
                                                                      
!       MONDAUFRUF  (GLEICHZEITIG RUECKGABE VON SONNENDATEN)            
        CALL UPMOND (C_BREITE,C_LAENGE,C_DATUM,HOEHE,LISTEM,LISTES,1,   
     &               MAW,MUW,TVM1,TVM2,TNM1,TNM2)                       
                                                                        
        IF(OUTPUT)  THEN                        
          WRITE (FU_OUTFIL,'(A)')                                              
     &           'MOND  REKT_HH DEK_GRD ABST_MM AZI_GRD HOE_GRD ELO_GRD'
          WRITE (FU_OUTFIL,FMT='(6X,6(F7.2,1X))') LISTEM(1),LISTEM(2),         
     &           LISTEM(7)*1.D-3,LISTEM(8),LISTEM(9),LISTEM(15)         
          WRITE (FU_OUTFIL,'(A)')                                              
     &           'MOND  AUFG_HH UNTG_HH PHAS_%  TAGE+/- NEU/VOL'        
          WRITE (FU_OUTFIL,FMT='(6X,5(F7.2,1X))') LISTEM(13),LISTEM(14),       
     &           LISTEM(16),LISTEM(17),LISTEM(18)                       
          WRITE (FU_OUTFIL,'(A)')                                              
     &           'SONNE REK_GRD DEK_GRD ABST_GM AZI_GRD HOE_GRD TAU_GRD'
          WRITE (FU_OUTFIL,FMT='(6X,6(F7.2,1X))') LISTES(1),LISTES(2),         
     &           LISTES(7)*1.D-6,LISTES(8),LISTES(9),LISTES(6)          
          WRITE (FU_OUTFIL,'(A)')                                              
     &           'SONNE AUFG_HH UNTG_HH'                                
          WRITE (FU_OUTFIL,FMT='(6X,2(F7.2,1X))') LISTES(13),LISTES(14)        
        ENDIF                                                           
                                                                        
!       SPEICHERUNG DER BENOETIGTEN WERTE                               
                                                                        
!       MONDPHASE > MPHASE                                              
        MPHASE = SNGL(LISTEM(16))                                       
                                                                        
!       MONDHOEHE  > MHOEH UND MOON(I,1)                                
        MHOEH=SNGL(LISTEM(9))                                           
        MHOPA=SNGL(LISTEM(10))                                          
        IF(I.NE.IOLD) MOON(I,1) = SNGL(LISTEM(9))                         
        HELDAT(IHELDA,9) = SNGL(LISTEM(9))                         
                                                                        
!       SONNENHOEHE > SHOEH                                             
        SHOEH=SNGL(LISTES(9) )                                          
                                                                        
!       MONDAZIMUT  > MOON(I,2)                                         
        IF(I.NE.IOLD) MOON(I,2) = SNGL(LISTEM(8))
        IF(HELDAT(IHELDA,9) > 0.0) HELDAT(IHELDA,10) = SNGL(LISTEM(8))
                                                                        
!       MONDELONGATION  > ELONG                                         
        ELONG = SNGL(LISTEM(15))                                        
                                                                        
!       MONDREKTASZENSION (AKTUELLE UHRZEIT) [MONREK]                   
        MONREK = SNGL(LISTEM(1))*15.                                    
                                                                        
!       MONDDEKLINATION (AKTUELLE UHRZEIT) [MONDEK]                     
        MONDEK = SNGL(LISTEM(2))                                        
                                                                        
!       ENTFERNUNG ERDE-MOND [KM] [ENTME]                               
        ENTME = SNGL(LISTEM(7))                                         
                                                                        
!       SONNENREKTASZENSION (AKTUELLE UHRZEIT) [SONREK]                 
        SONREK = SNGL(LISTES(1))                                        
                                                                        
!       SONNENDEKLINATION (AKTUELLE UHRZEIT) [SONDEK]                   
        SONDEK = SNGL(LISTES(2))                                        
                                                                        
!       ENTFERNUNG ERDE-SONNE [KM] [ENTSE]                              
        ENTSE = SNGL(LISTES(7))                                         
                                                                        
        BED08=0.                                                        
        BED88=8.                                                        
                                                                        
!       AUFRUF FINSTERNIS-UNTERPROGRAMM (BERECHNUNG DER REDUKTION       
!       DER HELLIGKEIT IN PROZENT BEI EVENT. FINSTERNIS)                
!       BEDMO=BEDECKUND DES MONDES INSGESAMT                            
                                                                        
        CALL FINST5(LISTES,LISTEM,IART,BEDMO)  
                                                                        
        FINSTR=1.                                                       
        IF(IART.EQ.2) THEN                                              
!         ANTEIL UNBEDECKTER MOND BEI FINSTERNIS                        
          FINSTR = BEDMO/100.                                           
        ENDIF                                                           
                                                                        
!       HELLIGKEIT BEI 0/8 BEDECKUNG [LX]                               
        CALL NHELIG4(SHOEH,MHOPA,MPHASE,ENTME,BED08,FINSTR,                   
     &               HELLIG,GBS,GBM,GRND,HELMAX)                  
        GA08 = HELLIG *1000.                                            
        IF (OUTPUT) THEN                        
          WRITE (FU_OUTFIL,FMT='(2(A,F8.2))')                                  
     &              'H[MLX] ANTEIL MOND:',GBM*1000.,'  SONNE:',GBS*1000.
        ENDIF                                                           
        IF(I.NE.IOLD) DATENR(I,13) = GA08                               
                                                                        
!       HELLIGKEIT BEI 8/8 BEDECKUNG [LX]                               
        CALL NHELIG4(SHOEH,MHOPA,MPHASE,ENTME,BED88,FINSTR,
     &               HELLIG,GBS,GBM,GRND,HELMAX)                  
        GA88 = HELLIG *1000.                                            
        IF(I.NE.IOLD) DATENR(I,14) = GA88                               
                                                                        
!       HELLIGKEIT BEI AKT/8 BEDECKUNG (PREVAILING) [LX]                
        BEDECK=(DATENR(I,3))                                            
        CALL NHELIG4(SHOEH,MHOPA,MPHASE,ENTME,BEDECK,FINSTR,                  
     &               HELLIG,GBS,GBM,GRND,HELMAX)                  
        GAKT1 = HELLIG*1000.                                            
                                                                        
!       HELLIGKEIT BEI AKT/8 BEDECKUNG (MINIMA) [LX]                    
        BEDECK=DATENR(I,4)                                              
        CALL NHELIG4(SHOEH,MHOPA,MPHASE,ENTME,BEDECK,FINSTR,                  
     &               HELLIG,GBS,GBM,GRND,HELMAX)                  
        GAKT2 = HELLIG*1000.                                            
                                                                        
        IF (OUTPUT) THEN                        
          WRITE (FU_OUTFIL,FMT='("H[MLX] (0,8,P,M) ",4F8.4)')                  
     &              GA08,GA88,GAKT1,GAKT2                               
        ENDIF                                                           
!       SELECT MONDHOEHE > NULL   
        I2 = 0
        IF(MHOEH.GT.0) THEN                                             
!         BERECHNUNG DER BLENDWIRKUNG JE NACH MONDHOEHE/HELLIGKEIT      
          CALL NHELIG4(SHOEH,MHOPA,MPHASE,ENTME,BED08,FINSTR,                 
     &                 HELLIG,GBS,GBM,GRND,HELMAX)                
          I1 = MAX(1,MIN(NINT(MHOEH)+1,60))                             
          I2 = MAX(1,MIN(NINT(GBM/HELMAX*100.)+1,60))                   
          IF (OUTPUT) THEN                      
            WRITE (FU_OUTFIL,FMT='("BLENDWIRKUNG,%",I3,F7.2,F8.2)')            
     &                BLEND(I1,I2),GBM/HELMAX*100.                      
          ENDIF                
          IF(I.NE.IOLD) MOON(I,3) = GBM/HELMAX*100.
          IF(I.NE.IOLD) MOON(I,4) = BLEND(I1,I2)                        
          HELDAT(IHELDA,8)        = GBM/HELMAX*100.
        ELSE                                                            
          IF(I.NE.IOLD) MOON(I,1) = 0.                                  
          IF(I.NE.IOLD) MOON(I,2) = 0.                                  
          IF(I.NE.IOLD) MOON(I,3) = 0.                                  
          IF(I.NE.IOLD) MOON(I,4) = 0.              
          HELDAT(IHELDA,8)        = 0.
!         SELECT-END                                                    
        ENDIF                        
                                                                        
!       BERECHNUNG KORREKTURFAKTOR ERDBODEN                             
        IF(DATENC(I,3)(1:1).EQ.'F'.OR.                                  
     &     DATENC(I,3)(1:1).EQ.'f') THEN                                
          E1= 2                                                         
        ELSEIF (DATENC(I,3)(1:1).EQ.'N'.OR.                             
     &          DATENC(I,3)(1:1).EQ.'n') THEN                           
          E1= 3                                                         
        ELSEIF (DATENC(I,3)(1:1).EQ.'*')THEN                            
          E1= 4                                                         
        ELSE                                                            
          E1= 1                                                         
        ENDIF                                                           

!       BERECHNUNG KORREKTURFAKTOR - PREVAILING (N,WW)                  
        W1 = INT(DATENR(I,6))                                           
        N1 = INT(DATENR(I,3))                                           
        IF (KORMAN(I,1).GT.0) THEN
          KORR=KORMAN(I,1)
        ELSE
          CALL KORFAK(W1,E1,N1,KORR)                                      
        ENDIF
        GAKT1 =  GAKT1 * KORR    
        IF (OUTPUT)
     &   WRITE(FU_OUTFIL,*) 
     &   'KORR, GAKT1*KORR,W1,E1,N1',KORR,GAKT1,W1,E1,N1
!        IF(I.NE.IOLD) DATENR(I,9)= GAKT1                                
        DATENR(I,9) = MIN(DATENR(I,9),GAKT1)
                                                                        
!       PREVAILING                                                      
!       NUR WENN SICHTWEITE >= 0 UND <= 15                              
        IF (DATENR(I,5).GT.15.) DATENR(I,5)=15.            
        IF (DATENR(I,5).GT.0.) THEN                                     
!         BIV-BRILLE 3.GENERATION                                       
          IF(GEN.EQ.3) THEN                                             
            CHECK=ALOG(20*GAKT1)                                        
            IF(CHECK.LT.1.E-7) CHECK=1.E-7                              
            R1 = EXP(-.869 + .624*ALOG(DATENR(I,5)) +                   
     &                .417* ALOG(CHECK))                                
            R2 = EXP(-.151 + .0247*ALOG(DATENR(I,5)) +                  
     &                .707* ALOG(CHECK))                                
            IF(R1.LT.R2) THEN                                           
!             IF(I.NE.IOLD) DATENR(I,11) = R1                           
              DATENR(I,11) = MIN(R1,DATENR(I,11))
              BIVRW1=R1                                                 
            ELSE                                                        
!              IF(I.NE.IOLD) DATENR(I,11) = R2                           
              DATENR(I,11) = MIN(R2,DATENR(I,11))
              BIVRW1=R2                                                 
            ENDIF  
            IF (OUTPUT)
     &        WRITE(FU_OUTFIL,*) '3. GEN. REICHW 1 U 2',R1,R2                    
!         BIV-BRILLE 2.GENERATION                                       
          ELSE                                                          
            CHECK=ALOG(10*GAKT1)                                        
            IF(CHECK.LT.1.E-7) CHECK=1.E-7                              
            R1 = EXP(-.869 + .624*ALOG(DATENR(I,5)) +                   
     &                .417* ALOG(ALOG(10*GAKT1)))                       
            R2 = EXP(-.151 + .0247*ALOG(DATENR(I,5)) +                  
     &                .707* ALOG(ALOG(10*GAKT1)))                       
            IF(R1.LT.R2) THEN                                           
!              IF(I.NE.IOLD) DATENR(I,11) = R1                           
              DATENR(I,11) = MIN(R1,DATENR(I,11))
              BIVRW1=R1                                                 
            ELSE                                                        
!              IF(I.NE.IOLD) DATENR(I,11) = R2                           
              DATENR(I,11) = MIN(R2,DATENR(I,11))
              BIVRW1=R2                                                 
            ENDIF                                                       
          ENDIF
        ELSE
          BIVRW1=0
        ENDIF                                                           
                                                                        
!       BERECHNUNG KORREKTURFAKTOR - MINIMUM (N,WW)                     
        IF(DATENC(I,6)(1:1).EQ.'F'.OR.                                  
     &     DATENC(I,6)(1:1).EQ.'f') THEN                                
          E1= 2                                                         
        ELSEIF (DATENC(I,6)(1:1).EQ.'N'.OR.                             
     &          DATENC(I,6)(1:1).EQ.'n') THEN                           
          E1= 3                                                         
        ELSEIF (DATENC(I,6)(1:1).EQ.'*')THEN                            
          E1= 4                                                         
        ELSE                                                            
          E1= 1                                                         
        ENDIF                                                           
                                                                        
        W1 = INT(DATENR(I,8))                                           
        N1 = INT(DATENR(I,4))                                           
        IF (KORMAN(I,2).GT.0) THEN
          KORR=KORMAN(I,2)
        ELSE
          CALL KORFAK(W1,E1,N1,KORR)                                      
        ENDIF
        GAKT2 = GAKT2 * KORR
        IF (OUTPUT)
     &    WRITE(FU_OUTFIL,*) 'KORR, GAKT2*KORR,W1,E1,N1',
     &    KORR,GAKT2,W1,E1,N1                                            
!        IF(I.NE.IOLD) DATENR(I,10) = GAKT2
        DATENR(I,10) = MIN(DATENR(I,10),GAKT2)
        
!       MINIMUM                                                         
!       NUR WENN SICHTWEITE >= 0 UND <= 15                              
        IF (DATENR(I,7).GT.15.) DATENR(I,7)=15.                         
!                                                                       
        IF (DATENR(I,7).GT.0.) THEN                                     
!         BIV-BRILLE 3.GENERATION                                       
          IF(GEN.EQ.3) THEN                                             
            CHECK=ALOG(20*GAKT2)                                        
            IF(CHECK.LT.1.E-7) CHECK=1.E-7
            R1 = EXP(-.869 + .624*ALOG(DATENR(I,7)) +                   
     &                .417* ALOG(CHECK))                       
            R2 = EXP(-.151 + .0247*ALOG(DATENR(I,7)) +                  
     &                .707* ALOG(CHECK))                       
            IF(R1.LT.R2) THEN                                           
!              IF(I.NE.IOLD) DATENR(I,12) = R1                           
              DATENR(I,12) = MIN(R1,DATENR(I,12))
              BIVRW2=R1                                                 
            ELSE                                                        
!              IF(I.NE.IOLD) DATENR(I,12) = R2                           
              DATENR(I,12) = MIN(R2,DATENR(I,12))
              BIVRW2=R2                                                 
            ENDIF                                                       
!         BIV-BRILLE 2.GENERATION                                       
          ELSE                                                          
            CHECK=ALOG(10*GAKT2)
            IF(CHECK.LT.1.E-7) CHECK=1.E-7                              
            R1 = EXP(-.869 + .624*ALOG(DATENR(I,7)) +                   
     &                .417* ALOG(ALOG(10*GAKT2)))                       
            R2 = EXP(-.151 + .0247*ALOG(DATENR(I,7)) +                  
     &                .707* ALOG(ALOG(10*GAKT2)))                       
            IF(R1.LT.R2) THEN                                           
!              IF(I.NE.IOLD) DATENR(I,12) = R1                           
              DATENR(I,12) = MIN(R1,DATENR(I,12))
              BIVRW2=R1                                                 
            ELSE                                                        
!              IF(I.NE.IOLD) DATENR(I,12) = R2
              DATENR(I,12) = MIN(R2,DATENR(I,12))              
              BIVRW2=R2                                                 
            ENDIF                                                       
          ENDIF                                                         
        ELSE
          BIVRW2=0
        ENDIF                                                           
          IF (OUTPUT) THEN                      
            WRITE (FU_OUTFIL,FMT='("REICHW.(P/M) ",2(F8.2,1X))')               
     &                     BIVRW1,BIVRW2                                
          ENDIF                                                         
                                                                        
!       FELD FUER PLOTT BELEGEN                                         
        HELDAT(IHELDA,1)=GAKT1                                          
        HELDAT(IHELDA,2)=GAKT2             
!       Vorgabe auf Grund von Vorschrift        
        IF(GAKT1.LT.0.5.AND.BIVRW1.GE.1.5) BIVRW1=1.49                                    
        HELDAT(IHELDA,3)=BIVRW1
!       Vorgabe auf Grund von Vorschrift                
        IF(GAKT2.LT.0.5.AND.BIVRW2.GE.1.5) BIVRW2=1.49                                    
        HELDAT(IHELDA,4)=BIVRW2                                         
        HELDAT(IHELDA,5)=GA08                                           
        HELDAT(IHELDA,6)=GA88
        HELDAT(IHELDA,7)=MPHASE        
        HELDATT(IHELDA)=PLTTIM
! 
        CALL DAYDAT(PLTTIM,YER,MON,DAY,ITIM,JTIM)
        
!       Typumwandlung VAR in STRING für HELDAT-Datei
        IGA08=INT(GA08)
        JGA08=INT(GA08*100)-IGA08*100
        IGA88=INT(GA88)
        JGA88=INT(GA88*100)-IGA88*100
        IGAKT1=INT(GAKT1)
        JGAKT1=INT(GAKT1*100)-IGAKT1*100
        IGAKT2=INT(GAKT2)
        JGAKT2=INT(GAKT2*100)-IGAKT2*100
        IBIVRW1=INT(BIVRW1)
        JBIVRW1=INT(BIVRW1*100)-IBIVRW1*100
        IBIVRW2=INT(BIVRW2)
        JBIVRW2=INT(BIVRW2*100)-IBIVRW2*100
        
        IF (.NOT.IS_WEB_APPL) THEN  
          WRITE(FU_HELDAT,1) 
     &      ITIM,':',JTIM,';',IGA08,JGA08,';',IGA88,JGA88,';',
     &      IGAKT1,JGAKT1,';',IGAKT2,JGAKT2,';',
     &      IBIVRW1,JBIVRW1,';',IBIVRW2,JBIVRW2 
    1   FORMAT(I2.2,A,I2.2,A,6(I8,',',I2.2,A))         
        ENDIF
!       Typumwandlung VAR in STRING für MOONDAT-Datei
        IA=INT(LISTEM(8))
        JA=INT(LISTEM(8)*100)-IA*100
        IF (LISTEM(9).LT.0) THEN
           NH=-1
        ELSE
           NH=1
        ENDIF
        IH=INT(DABS(LISTEM(9)))
        JH=INT(DABS(LISTEM(9))*100)-IH*100
       
        IL=INT(GBM/HELMAX*100.)
        JL=INT(GBM/HELMAX*100.*100)-IL*100
        
        IF (.NOT.IS_WEB_APPL) THEN
          WRITE(FU_MOONDAT,2) 
     &      ITIM,':',JTIM,';',IA,JA,';',NH*IH,JH,';', 
     &      BLEND(I1,I2),';',IL,JL                                         
    2     FORMAT(I2.2,A,I2.2,A,2(I3,',',I2.2,A),I2,A,I3,',',I2.2,A)    
        ENDIF

!       ZEITSCHRITT ERHOEHEN                                            
        IOLD=I               
        PLTTIM = PLTTIM + JSTEP
        IF (PLTTIM > (END_TIMES(I)-JSTEP2)) THEN
          I=I+1
        ENDIF
       
      ENDDO 
      
      IF (.NOT.IS_WEB_APPL) THEN   
        CLOSE(FU_HELDAT)
        CLOSE(FU_MOONDAT)
      ENDIF 
      
!     Ausgabedatei schliessen      
      IF (OUTPUT) CLOSE(FU_OUTFIL)            
      END                                                               
                                                                        