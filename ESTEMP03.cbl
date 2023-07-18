      IDENTIFICATION DIVISION.
     *-----------------------
      PROGRAM-ID. ESTEMP03.
     *AUTHOR. SEBASTIAN SASSONE.
     *DATE-WRITTEN. 10/06/2023.
     *DATE-COMPILED. EARRING.
     *----------------------------------------------------------------
     *ESTE ES UN PROGRAMA LISTADOR DE RESGISTROS DE TEMPERATURAS
     *UN ARCHIVO DE INPUT Y UN ARCHIVO DE OUPUT
     *----------------------------------------------------------------

      ENVIRONMENT DIVISION.
     *--------------------

      CONFIGURATION SECTION.
     *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.                                      
                                                                        

      INPUT-OUTPUT SECTION.
     *--------------------

      FILE-CONTROL.
     *------------

          SELECT MAEDISP         ASSIGN       TO MAEDISP               
                                 ORGANIZATION IS INDEXED               
                                 ACCESS MODE  IS RANDOM                
                                 RECORD KEY   IS REG-MAEDISP-KEY-FD    
                                 FILE STATUS  IS FS-MAEDISP.           
                                                                       
          SELECT LISTADO         ASSIGN       TO LISTADO               
                                 ORGANIZATION IS SEQUENTIAL
                                 FILE STATUS  IS FS-LISTADO.

      DATA DIVISION.                                                   
     *-------------                                                    
                                                               
      FILE SECTION.                                            
     *------------                                             
                                                               
      FD  MAEDISP.                                             
      01  ENT-REG-MAEDISP.                                     
          03 REG-MAEDISP-KEY-FD          PIC X(05).            
          03 FILLER                      PIC X(75).            
                                                               
      FD  LISTADO                                              
          RECORDING MODE IS F                                  
          BLOCK 0.                                             
      01  REG-LISTADO-FD                 PIC X(160).           
                                                               
      WORKING-STORAGE SECTION.                                 
     *-----------------------                                  
      77  WS-SQLCODE                    PIC S9(4) VALUE ZEROES.
          88 DB2-OK                               VALUE +0.    
          88 DB2-NOTFND                           VALUE +100.      
          88 DB2-DUPREC                           VALUE -806.      
                                                                   
      77  WS2-SQLCODE                   PIC S9(4) VALUE ZERO.      
          88 DB2-OK2                              VALUE +0.        
          88 DB2-NOTFND2                          VALUE +100.      
          88 DB2-DUPREC2                          VALUE -803.      
                                                                   
      77  WS-OPEN-CUR-TREGTEMP          PIC X     VALUE 'N'.       
          88 88-FS-OPEN-CUR-SI                    VALUE 'S'.       
          88 88-FS-OPEN-CUR-NO                    VALUE 'N'.       
                                                                   
      77  FS-MAEDISP                    PIC X(02) VALUE ' '.       
          88 88-FS-MAEDISP-OK                     VALUE '00' '97'. 
          88 88-FS-MAEDISP-NFD                    VALUE '23'.      
                                                                   
      77  WS-OPEN-MAEDISP               PIC X     VALUE 'N'.       
          88 88-OPEN-MAEDISP-SI                   VALUE 'S'.       
          88 88-OPEN-MAEDISP-NO                   VALUE 'N'.       
                                                                    
      77  FS-LISTADO                    PIC X(02) VALUE ' '.        
          88 88-FS-LISTADO-OK                     VALUE '00'.       
                                                                    
      77  WS-OPEN-LISTADO               PIC X     VALUE 'N'.        
          88 88-OPEN-LISTADO-SI                   VALUE 'S'.        
          88 88-OPEN-LISTADO-NO                   VALUE 'N'.        
      77  WS-GRABADOS-LISTADO           PIC 9(09) VALUE 0.          
      77  WS-GRABADOS-LISTADO-ED        PIC 9(09) VALUE 0.          
      77  WS-TEMP-ED                    PIC ---9,9999.              
                                                                    
      77  WS-LINEAS                     PIC 9(02) VALUE 90.         
      77  WS-LINEAS-MAX                 PIC 9(02) VALUE 66.         
                                                                    
      01 WS-FECHA-TOMA.                                             
         03 WS-FECHA-DD                 PIC 99.                     
         03 FILLER                      PIC X VALUE '/'.            
         03 WS-FECHA-MM                 PIC 99.                     
         03 FILLER                      PIC X VALUE '/'.            
         03 WS-FECHA-AAAA               PIC 9(04).        
                                                          
      01 WS-HORA-ED.                                      
         03 WS-HORA-HH                   PIC 99.          
         03 FILLER                       PIC X VALUE ':'. 
         03 WS-HORA-MN                   PIC 99.          
         03 FILLER                       PIC X VALUE ':'. 
         03 WS-HORA-SS                   PIC 99.          
                                                          
      01 WS-LATLON-ED.                                    
         03 WS-LATLON-GRADOS             PIC 9(3).        
         03 FILLER                       PIC X VALUE '.'. 
         03 WS-LATLON-MINUTOS            PIC 99.          
         03 FILLER                       PIC X VALUE '.'. 
         03 WS-LATLON-SEGUNDOS           PIC 99.          
         03 FILLER                       PIC X VALUE '.'. 
         03 WS-LATLON-HEMISF             PIC X.           
                                                          
      01 WS1-FECHA-TOMA.     
         03 WS1-FECHA-DD                 PIC 99.            
         03 FILLER                       PIC X VALUE '/'.   
         03 WS1-FECHA-MM                 PIC 99.            
         03 FILLER                       PIC X VALUE '/'.   
         03 WS1-FECHA-AAAA               PIC 9(04).         
                                                            
      01 WS1-HORA-ED.                                       
         03 WS1-HORA-HH                  PIC 99.            
         03 FILLER                       PIC X VALUE ':'.   
         03 WS1-HORA-MN                  PIC 99.            
         03 FILLER                       PIC X VALUE ':'.   
         03 WS1-HORA-SS                  PIC 99.            
                                                            
      01 WS1-LATLON-ED.                                     
         03 WS1-LATLON-GRADOS            PIC 9(3).          
         03 FILLER                       PIC X VALUE '.'.   
         03 WS1-LATLON-MINUTOS           PIC 99.            
         03 FILLER                       PIC X VALUE '.'.   
         03 WS1-LATLON-SEGUNDOS          PIC 99.     
         03 FILLER                       PIC X VALUE '.'.     
         03 WS1-LATLON-HEMISF            PIC X.               
                                                              
      01 WS-CURRENT-DATE.                                     
          03 WS-CURRDATE-AAAA             PIC 9(4) VALUE 0.   
          03 WS-CURRDATE-MM               PIC 9(2) VALUE 0.   
          03 WS-CURRDATE-DD               PIC 9(2) VALUE 0.   
          03 WS-CURRDATE-HH               PIC 9(2) VALUE 0.   
          03 WS-CURRDATE-MN               PIC 9(2) VALUE 0.   
          03 WS-CURRDATE-SS               PIC 9(2) VALUE 0.   
                                                              
      01 DET01-LAT-HEM                    PIC X(01).          
      01 DET01-LONG-HEM                   PIC X(01).          
      01 TOTMAX-LAT-HEM                   PIC X(01).          
      01 TOTMAX-LONG-HEM                  PIC X(01).          
      01 TOTMIN-LAT-HEM                   PIC X(01).          
      01 TOTMIN-LONG-HEM                  PIC X(01).          
                                                              
     *---------------------------------------------      
     * ESTRUCTURA DEL ARCHIVO MAEDISP                            
     *---------------------------------------------              
      COPY WCODDIS.                                              
                                                                 
     *---------------------------------------------              
     * DEFINICON DE LAS TABLAS.                                  
     *---------------------------------------------              
          EXEC SQL                                               
            INCLUDE TREGTEMP                                     
          END-EXEC.                                              
                                                                 
          EXEC SQL                                               
            INCLUDE TESTATUS                                     
          END-EXEC.                                              
                                                                 
          EXEC SQL                                               
            INCLUDE TFABRICA                                     
          END-EXEC.                                              
                                                                      
     *---------------------------------------------                   
     * DEFINICON DEL AREA SQLCA.                                      
     *---------------------------------------------                   
          EXEC SQL                                                    
            INCLUDE SQLCA                                             
          END-EXEC.                                                   
     *---------------------------------------------                   
     * DEFINICION DE CURSOR DE LA TABLA TREGTEMP.                     
     *---------------------------------------------                   
          EXEC SQL                                                    
               DECLARE CURSOR-TREGTEMP CURSOR FOR                     
               SELECT CODDISP                                         
                     ,CODFAB                                          
                     ,FECTOMA                                         
                     ,HORTOMA                                         
                     ,TEMPERAT                                        
                     ,HUMEDAD                                         
                     ,LATGRAD                                         
                     ,LATMIN      
                     ,LATSEC                              
                     ,LATHEMIS                            
                     ,LONGRAD                             
                     ,LONMIN                              
                     ,LONSEC                              
                     ,LONHEMIS                            
                     ,CODESTAT                            
               FROM IDCC22.TREGTEMP                       
          END-EXEC.                                       
                                                          
     *---------------------------------------------       
     *AREA PARA LA TEMPERATURA MINIMA                     
     *---------------------------------------------       
                                                          
      01  TMIN-REGISTRO.                                  
          03 TMIN-COD-DISP       PIC 9(05).               
          03 TMIN-COD-FABRI      PIC 9(05).               
          03 TMIN-COD-ESTADO     PIC X(03).               
          03 TMIN-NOM-DISP       PIC X(30).    
          03 TMIN-NOM-FABRI      PIC X(05).               
          03 TMIN-DISP-EST-DESC  PIC X(30).               
          03 TMIN-HUMEDAD        PIC 9(03)V9(02).         
          03 TMIN-FEC.                                    
             05 TMIN-FEC-AAAA    PIC 9(04).               
             05 TMIN-FEC-MM      PIC 9(02).               
             05 TMIN-FEC-DD      PIC 9(02).               
          03 TMIN-HORA.                                   
             05 TMIN-HORA-HH     PIC 9(02).               
             05 TMIN-HORA-MM     PIC 9(02).               
             05 TMIN-HORA-SS     PIC 9(02).               
          03 TMIN-GRADOS         PIC S9(04)V9(04).        
          03 TMIN-LAT.                                    
             05 TMIN-LAT-GRAD    PIC 9(03).               
             05 TMIN-LAT-MIN     PIC 9(02).               
             05 TMIN-LAT-SEG     PIC 9(02).               
             05 TMIN-LAT-HEM     PIC X(01).               
          03 TMIN-LONG.                                   
             05 TMIN-LONG-GRAD   PIC 9(03).   
             05 TMIN-LONG-GRAD   PIC 9(03).              
             05 TMIN-LONG-MIN    PIC 9(02).              
             05 TMIN-LONG-SEG    PIC 9(02).              
             05 TMIN-LONG-HEM    PIC X(01).              
                                                         
     *-------------------------------------------------- 
     *AREA PARA LA TEMPERATURA MAXIMA                    
     *-------------------------------------------------- 
      01  TMAX-REGISTRO.                                 
          03 TMAX-COD-DISP       PIC 9(05).              
          03 TMAX-COD-FABRI      PIC 9(05).              
          03 TMAX-COD-ESTADO     PIC X(03).              
          03 TMAX-NOM-DISP       PIC X(30).              
          03 TMAX-NOM-FABRI      PIC X(05).              
          03 TMAX-DISP-EST-DESC  PIC X(30).              
          03 TMAX-HUMEDAD        PIC 9(03)V9(02).        
          03 TMAX-FEC.                                   
             05 TMAX-FEC-AAAA    PIC 9(04).              
             05 TMAX-FEC-MM      PIC 9(02).              
             05 TMAX-FEC-DD      PIC 9(02).        
          03 TMAX-HORA.                            
             05 TMAX-HORA-HH     PIC 9(02).        
             05 TMAX-HORA-MM     PIC 9(02).        
             05 TMAX-HORA-SS     PIC 9(02).        
          03 TMAX-GRADOS         PIC S9(04)V9(04). 
          03 TMAX-LAT.                             
             05 TMAX-LAT-GRAD    PIC 9(03).        
             05 TMAX-LAT-MIN     PIC 9(02).        
             05 TMAX-LAT-SEG     PIC 9(02).        
             05 TMAX-LAT-HEM     PIC X(01).        
          03 TMAX-LONG.                            
             05 TMAX-LONG-GRAD   PIC 9(03).        
             05 TMAX-LONG-MIN    PIC 9(02).        
             05 TMAX-LONG-SEG    PIC 9(02).        
             05 TMAX-LONG-HEM    PIC X(01).        
                                                   
     * DEFINICION DE LINEAS DE IMPRESION LISTADO   
     * -----------------------------------------   
      01 TIT01.                                    
      01 TIT01.                                                    
         03 FILLER                      PIC X(08) VALUE 'PROGRAM:'.
         03 TIT01-PROGRAMA              PIC X(08) VALUE 'ESTEMP01'.
         03 FILLER                      PIC X(38) VALUE ' '.       
         03 FILLER                      PIC X(27) VALUE            
                                     'RECORDED TEMPERATURE REPORT'.
         03 FILLER                      PIC X(37) VALUE ' '.       
         03 FILLER                      PIC X(06) VALUE 'PAGE: '.  
         03 TIT01-PAGINA                PIC 9(04).                 
                                                                   
      01 TIT02.                                                    
         03 FILLER                      PIC X(08) VALUE 'DATE   :'.
         03 TIT02-FECHA.                                           
            05 TIT02-FECHA-DD           PIC 99.                    
            05 FILLER                   PIC X(01) VALUE '-'.       
            05 TIT02-FECHA-MM           PIC 99.                    
            05 FILLER                   PIC X(01) VALUE '-'.       
            05 TIT02-FECHA-AAAA         PIC 9999.                  
         03 FILLER                      PIC X(36) VALUE ' '.       
         03 FILLER                      PIC X(27) VALUE ALL '-'.     
          03 FILLER                      PIC X(37) VALUE ' '.     
          03 FILLER                      PIC X(06) VALUE 'TIME :'.
          03 TIT02-HORA.                                          
             05 TIT02-HORA-HH            PIC 9(02).               
             05 FILLER                   PIC X(01) VALUE ':'.     
             05 TIT02-HORA-MM            PIC 9(02).               
             05 FILLER                   PIC X(01) VALUE ':'.     
             05 TIT02-HORA-SS            PIC 9(02).               
                                                                  
       01 TIT03                          PIC X(160) VALUE ' '.    
                                                                  
       01 TIT04.                                                  
          03 FILLER                    PIC X(46) VALUE ALL '-'.   
          03 FILLER                    PIC X(01) VALUE ' '.       
          03 FILLER                    PIC X(26) VALUE ALL '-'.   
          03 FILLER                    PIC X(01) VALUE ' '.       
          03 FILLER                    PIC X(19) VALUE ALL '-'.   
          03 FILLER                    PIC X(01) VALUE ' '.       
          03 FILLER                    PIC X(20) VALUE ALL '-'.     
         03 FILLER                    PIC X(01) VALUE ' '.           
         03 FILLER                    PIC X(18) VALUE ALL '-'.       
         03 FILLER                    PIC X(01) VALUE ' '.           
         03 FILLER                    PIC X(18) VALUE ALL '-'.       
                                                                     
      01 TIT05.                                                      
         03 FILLER                    PIC X(20) VALUE ' '.           
         03 FILLER                    PIC X(06) VALUE 'DEVICE'.      
         03 FILLER                    PIC X(28) VALUE ' '.           
         03 FILLER                    PIC X(12) VALUE 'MANUFACTERER'.
         03 FILLER                    PIC X(13) VALUE ' '.           
         03 FILLER                    PIC X(11) VALUE                
                                      'SAMPLE DATA'.                 
         03 FILLER                    PIC X(11) VALUE ' '.           
         03 FILLER                    PIC X(07) VALUE ALL 'WEATHER'. 
         03 FILLER                    PIC X(13) VALUE ' '.           
         03 FILLER                    PIC X(08) VALUE ALL 'LATITUDE'.
         03 FILLER                    PIC X(08) VALUE ' '.           
         03 FILLER                    PIC X(09) VALUE ALL 'LONGITUDE'   
                                                                  
      01 TIT06.                                                   
         03 FILLER                    PIC X(46) VALUE ALL '-'.    
         03 FILLER                    PIC X(01) VALUE ' '.        
         03 FILLER                    PIC X(26) VALUE ALL '-'.    
         03 FILLER                    PIC X(01) VALUE ' '.        
         03 FILLER                    PIC X(19) VALUE ALL '-'.    
         03 FILLER                    PIC X(01) VALUE ' '.        
         03 FILLER                    PIC X(20) VALUE ALL '-'.    
         03 FILLER                    PIC X(01) VALUE ' '.        
         03 FILLER                    PIC X(18) VALUE ALL '-'.    
         03 FILLER                    PIC X(01) VALUE ' '.        
         03 FILLER                    PIC X(18) VALUE ALL '-'.    
                                                                  
      01 TIT07.                                                   
         03 FILLER                    PIC X(02) VALUE 'ID'.       
         03 FILLER                    PIC X(04) VALUE ' '.        
         03 FILLER                    PIC X(04) VALUE 'NAME'.     
         03 FILLER                    PIC X(17) VALUE ' '.    
         03 FILLER                    PIC X(17) VALUE ' '.          
         03 FILLER                    PIC X(06) VALUE 'STATUS'.     
         03 FILLER                    PIC X(14) VALUE ' '.          
         03 FILLER                    PIC X(02) VALUE 'ID'.         
         03 FILLER                    PIC X(04) VALUE ' '.          
         03 FILLER                    PIC X(04) VALUE 'NAME'.       
         03 FILLER                    PIC X(20) VALUE ' '.          
         03 FILLER                    PIC X(04) VALUE 'DATE'.       
         03 FILLER                    PIC X(06) VALUE ' '.          
         03 FILLER                    PIC X(04) VALUE 'TIME'.       
         03 FILLER                    PIC X(03) VALUE ' '.          
         03 FILLER                    PIC X(11) VALUE 'TEMPERATURE'.
         03 FILLER                    PIC X(01) VALUE ' '.          
         03 FILLER                    PIC X(08) VALUE 'HUMIDITY'.   
         03 FILLER                    PIC X(01) VALUE ' '.          
         03 FILLER                    PIC X(03) VALUE 'GRA'.        
         03 FILLER                    PIC X(01) VALUE ' '.          
         03 FILLER                    PIC X(03) VALUE 'MIN'.        
         03 FILLER                    PIC X(01) VALUE ' '.          
         03 FILLER                    PIC X(03) VALUE 'SEC'.    
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(06) VALUE 'HEMISF'.   
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(03) VALUE 'GRA'.      
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(03) VALUE 'MIN'.      
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(03) VALUE 'SEC'.      
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(06) VALUE 'HEMISF'.   
                                                                 
     01 TIT08.                                                   
        03 FILLER                    PIC X(05) VALUE ALL '-'.    
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(20) VALUE ALL '-'.    
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(19) VALUE ALL '-'.    
        03 FILLER                    PIC X(01) VALUE ' '.        
        03 FILLER                    PIC X(05) VALUE ALL '-'. 
         03 FILLER                    PIC X(05) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(20) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(10) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(08) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(11) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(08) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(03) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(03) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(03) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(06) VALUE ALL '-'. 
         03 FILLER                    PIC X(01) VALUE ' '.     
         03 FILLER                    PIC X(03) VALUE ALL '-'.   
         03 FILLER                    PIC X(01) VALUE ' '.       
         03 FILLER                    PIC X(03) VALUE ALL '-'.   
         03 FILLER                    PIC X(01) VALUE ' '.       
         03 FILLER                    PIC X(03) VALUE ALL '-'.   
         03 FILLER                    PIC X(01) VALUE ' '.       
         03 FILLER                    PIC X(06) VALUE ALL '-'.   
                                                                 
      01 DET01.                                                  
          03 DET01-COD-DISPO          PIC X(05).                 
          03 FILLER                   PIC X(01) VALUE ' '.       
          03 DET01-NOMBRE-DISPO       PIC X(20).                 
          03 FILLER                   PIC X(01) VALUE ' '.       
          03 DET01-COD-ESTADO         PIC X(03).                 
          03 FILLER                   PIC X(01) VALUE ' '.       
          03 DET01-COD-ESTADO-DESCR   PIC X(15).                 
          03 FILLER                   PIC X(01) VALUE ' '.       
          03 DET01-COD-FABRICANTE     PIC X(05).                 
          03 FILLER                   PIC X(01) VALUE ' '.    

          03 DET01-TEMPERATURA        PIC -----9,9999.         
          03 FILLER                   PIC X(02) VALUE '  '.    
          03 DET01-HUMEDAD            PIC ZZ9,99.              
          03 FILLER                   PIC X(02) VALUE '  '.    
                                                               
          03 DET01-LATITUD.                                    
             05 DET01-LAT-GRA         PIC ZZ9.                 
             05 FILLER                PIC X(02) VALUE '  '.    
             05 DET01-LAT-MIN         PIC Z9.                  
             05 FILLER                PIC X(02) VALUE '  '.    
             05 DET01-LAT-SEC         PIC Z9.                  
             05 FILLER                PIC X(01) VALUE ' '.     
             05 DET01-LAT-HEMIS-DESC     PIC X(06).            
             05 FILLER                PIC X(01) VALUE ' '.     
          03 DET01-LONG.                                       
             05 DET01-LONG-GRA        PIC ZZ9.                 
             05 FILLER                PIC X(02) VALUE '  '.    
             05 DET01-LONG-MIN        PIC Z9.                  
             05 FILLER                PIC X(02) VALUE '  '.    
             05 DET01-LONG-SEC        PIC Z9.                      
             05 FILLER                PIC X(01) VALUE ' '.         
             05 DET01-LONG-HEMIS-DESC PIC X(06).                   
                                                                   
      01 TIT09                        PIC X(160) VALUE ' '.        
                                                                   
      01 TOTMAX-01.                                                
          03 FILLER                   PIC X(09) VALUE 'TEMP MAX:'. 
      01 DETMAX.                                                   
          03 TOTMAX-COD-DISPO         PIC X(05).                   
          03 FILLER                   PIC X(01) VALUE ' '.         
          03 TOTMAX-NOMBRE-DISPO      PIC X(20).                   
          03 FILLER                   PIC X(01) VALUE ' '.         
          03 TOTMAX-COD-ESTADO        PIC X(03).                   
          03 FILLER                   PIC X(01) VALUE ' '.         
          03 TOTMAX-COD-ESTADO-DESCR  PIC X(15).                   
          03 FILLER                   PIC X(01) VALUE ' '.         
          03 TOTMAX-COD-FABRICANTE    PIC X(05).                   
          03 FILLER                   PIC X(01) VALUE ' '.      
          03 TOTMAX-NOM-FABRICANTE    PIC X(20).               
          03 FILLER                   PIC X(01) VALUE ' '.     
                                                               
          03 TOTMAX-FECHA.                                     
             05 TOTMAX-FECHA-DD       PIC X(02).               
             05 FILLER                PIC X VALUE '-'.         
             05 TOTMAX-FECHA-MM       PIC X(02).               
             05 FILLER                PIC X VALUE '-'.         
             05 TOTMAX-FECHA-AAAA     PIC X(04).               
          03 FILLER                   PIC X(01) VALUE ' '.     
                                                               
          03 TOTMAX-HORA-TOMA.                                 
             05 TOTMAX-HH             PIC 9(02).               
             05 FILLER                PIC X VALUE ':'.         
             05 TOTMAX-MM             PIC 9(02).               
             05 FILLER                PIC X VALUE ':'.         
             05 TOTMAX-SS             PIC 9(02).               
          03 FILLER                   PIC X(01) VALUE ' '. 
                                                             
          03 TOTMAX-TEMPERATURA       PIC -----9,9999.       
          03 FILLER                   PIC X(02) VALUE '  '.  
          03 TOTMAX-HUMEDADPIC        PIC ZZ9,99.            
          03 FILLER                   PIC X(02) VALUE '  '.  
                                                             
          03 TOTMAX-LATITUD.                                 
             05 TOTMAX-LAT-GRA        PIC ZZ9.               
             05 FILLER                PIC X(02) VALUE '  '.  
             05 TOTMAX-LAT-MIN        PIC Z9.                
             05 FILLER                PIC X(02) VALUE '  '.  
             05 TOTMAX-LAT-SEC        PIC Z9.                
             05 FILLER                PIC X(01) VALUE ' '.   
             05 TOTMAX-LAT-HEMIS-DES  PIC X(06).             
             05 FILLER                PIC X(01) VALUE ' '.   
                                                             
          03 TOTMAX-LONG.                                    
             05 TOTMAX-LONG-GRA       PIC ZZ9.               
             05 FILLER                PIC X(02) VALUE '  '.    
             05 TOTMAX-LONG-MIN       PIC Z9.                     
             05 FILLER                PIC X(02) VALUE '  '.       
             05 TOTMAX-LONG-SEC       PIC Z9.                     
             05 FILLER                PIC X(01) VALUE ' '.        
             05 TOTMAX-LONG-HEMIS-DES PIC X(06).                  
                                                                  
      01 TIT010                       PIC X(160) VALUE ' '.       
                                                                  
      01 TOTMIN.                                                  
          03 FILLER                   PIC X(09) VALUE 'TEMP MIN:'.
      01 DETMIN.                                                  
          03 TOTMIN-COD-DISPO         PIC X(05).                  
          03 FILLER                   PIC X(01) VALUE ' '.        
          03 TOTMIN-NOMBRE-DISPO      PIC X(20).                  
          03 FILLER                   PIC X(01) VALUE ' '.        
          03 TOTMIN-COD-ESTADO        PIC X(03).                  
          03 FILLER                   PIC X(01) VALUE ' '.        
          03 TOTMIN-COD-ESTADO-DESCR  PIC X(15).                  
          03 FILLER                   PIC X(01) VALUE ' '.  
          03 TOTMIN-COD-FABRICANTE    PIC X(05).             
          03 FILLER                   PIC X(01) VALUE ' '.   
          03 TOTMIN-NOM-FABRICANTE    PIC X(20).             
          03 FILLER                   PIC X(01) VALUE ' '.   
                                                             
          03 TOTMIN-FECHA.                                   
             05 TOTMIN-FECHA-DD       PIC X(02).             
             05 FILLER                PIC X VALUE '-'.       
             05 TOTMIN-FECHA-MM       PIC X(02).             
             05 FILLER                PIC X VALUE '-'.       
             05 TOTMIN-FECHA-AAAA     PIC X(04).             
          03 FILLER                   PIC X(01) VALUE ' '.   
                                                             
          03 TOTMIN-HORA-TOMA.                               
             05 TOTMIN-HH             PIC 9(02).             
             05 FILLER                PIC X VALUE ':'.       
             05 TOTMIN-MM             PIC 9(02).             
             05 FILLER                PIC X VALUE ':'.   
              05 TOTMIN-SS             PIC 9(02).               
           03 FILLER                   PIC X(01) VALUE ' '.     
                                                                
           03 TOTMIN-TEMPERATURA       PIC -----9,9999.         
           03 FILLER                   PIC X(02) VALUE '  '.    
           03 TOTMIN-HUMEDAD           PIC ZZ9,99.              
           03 FILLER                   PIC X(02) VALUE '  '.    
                                                                
           03 TOTMIN-LATITUD.                                   
              05 TOTMIN-LAT-GRA        PIC ZZ9.                 
              05 FILLER                PIC X(02) VALUE '  '.    
              05 TOTMIN-LAT-MIN        PIC Z9.                  
              05 FILLER                PIC X(02) VALUE '  '.    
              05 TOTMIN-LAT-SEC        PIC Z9.                  
              05 FILLER                PIC X(01) VALUE ' '.     
              05 TOTMIN-LAT-HEMIS-DES  PIC X(06).               
              05 FILLER                PIC X(01) VALUE ' '.     
                                                                
           03 TOTMIN-LONG.     
              05 TOTMIN-LONG-GRA       PIC ZZ9.                      
              05 FILLER                PIC X(02) VALUE '  '.         
              05 TOTMIN-LONG-MIN       PIC Z9.                       
              05 FILLER                PIC X(02) VALUE '  '.         
              05 TOTMIN-LONG-SEC       PIC Z9.                       
              05 FILLER                PIC X(01) VALUE ' '.          
              05 TOTMIN-LONG-HEMIS-DES PIC X(06).                    
                                                                     
      * ESTRUCTURA DEL ARCHIVO DE SALIDA.                            
      * --------------------------------                             
       01  REG-LISTADO                PIC X(160).                    
                                                                     
       PROCEDURE DIVISION.                                           
      *------------------                                            
                                                                     
       00000-CUERPO-PRINCIPAL.                                       
      *----------------------                                        
                                                                     
           PERFORM 10000-INICIO.  
                                                                   
          PERFORM 20000-PROCESO                                    
            UNTIL 88-FS-OPEN-CUR-NO.                               
                                                                   
          PERFORM 30000-FINALIZO.                                  
                                                                   
          STOP RUN.                                                
                                                                   
      10000-INICIO.                                                
     *-------------                                                
                                                                   
          PERFORM 10100-ABRO-ARCHIVOS.                             
                                                                   
          PERFORM 10500-LEO-CURSOR-TREGTEMP.                       
                                                                   
      10100-ABRO-ARCHIVOS.                                         
     *-------------------                                          
                                                                   
          PERFORM 10200-ABRO-CURSOR-TREGTEMP.    
          PERFORM 10300-ABRO-MAEDISP.                                  
                                                                       
          PERFORM 10400-ABRO-LISTADO.                                  
                                                                       
      10200-ABRO-CURSOR-TREGTEMP.                                      
     *--------------------------                                       
          EXEC SQL                                                     
            OPEN CURSOR-TREGTEMP                                       
          END-EXEC.                                                    
                                                                       
          MOVE SQLCODE                  TO WS-SQLCODE.                 
                                                                       
          EVALUATE TRUE                                                
               WHEN DB2-OK                                             
                    CONTINUE                                           
               WHEN OTHER                                              
                    DISPLAY 'OCURRIO UN ERROR AL ABRIR CURSOR-TREGTEMP'
                    DISPLAY 'SQL-CODE: ' WS-SQLCODE                    
          END-EVALUATE.     
                                                                       
      10300-ABRO-MAEDISP.                                              
     *-------------------                                              
                                                                       
          OPEN INPUT MAEDISP.                                          
                                                                       
          EVALUATE FS-MAEDISP                                          
              WHEN '00'                                                
                   SET 88-OPEN-MAEDISP-SI TO TRUE                      
                                                                       
              WHEN OTHER                                               
                   DISPLAY 'ERROR OPEN MAEDISP FS: ' FS-MAEDISP        
                   STOP RUN                                            
          END-EVALUATE.                                                
                                                                       
      10400-ABRO-LISTADO.                                              
     *-------------------                                              
                                                                       
          OPEN OUTPUT  LISTADO.   
                                                                       
          EVALUATE FS-LISTADO                                          
              WHEN '00'                                                
                   SET 88-OPEN-LISTADO-SI TO TRUE                      
                                                                       
              WHEN OTHER                                               
                   DISPLAY 'ERROR OPEN LISTADO FS: ' FS-LISTADO        
                   STOP RUN                                            
                                                                       
          END-EVALUATE.                                                
                                                                       
      10500-LEO-CURSOR-TREGTEMP.                                       
     *-------------------------                                        
                                                                       
          PERFORM 23500-FETCH-TREGTEM.                                 
                                                                       
          IF 88-FS-OPEN-CUR-NO                                         
             DISPLAY ' '                                               
             DISPLAY 'NO HAY INFORMACION EN LA TABLA'    
         ELSE                                                        
            PERFORM 26000-CARGO-DET01                                
            PERFORM 24000-CARGO-MINIMA                               
            PERFORM 25000-CARGO-MAXIMA                               
         END-IF.                                                     
                                                                     
     12100-LEO-MAEDISP.                                              
    *------------------                                              
                                                                     
         INITIALIZE REG-MAEDISP                                      
                                                                     
         MOVE H-CODDSP           TO  REG-MAEDISP-KEY-FD.             
                                                                     
         READ MAEDISP INTO REG-MAEDISP.                              
                                                                     
         EVALUATE TRUE                                               
             WHEN 88-FS-MAEDISP-OK                                   
                  MOVE MAEDISP-DESCR   TO DET01-NOMBRE-DISPO         
                  MOVE MAEDISP-DESCR   TO TOTMIN-NOMBRE-DISPO       
                    MOVE MAEDISP-DESCR   TO TOTMAX-NOMBRE-DISPO      
               WHEN 88-FS-MAEDISP-NFD                                
                    DISPLAY 'CLAVE NO ENCONTRADA ' REG-MAEDISP-KEY-FD
                    MOVE 'SIN NOMBRE'    TO DET01-NOMBRE-DISPO       
                    MOVE 'SIN NOMBRE'    TO TOTMIN-NOMBRE-DISPO      
                    MOVE 'SIN NOMBRE'    TO TOTMAX-NOMBRE-DISPO      
                                                                     
               WHEN OTHER                                            
                    DISPLAY 'ERROR EN READ MAEDISP FS: ' FS-MAEDISP  
                    STOP RUN                                         
           END-EVALUATE.                                             
                                                                     
       20000-PROCESO.                                                
      *-------------                                                 
                                                                     
           PERFORM 26000-CARGO-DET01.                                
                                                                     
           IF H-TEMPERAT  <= TMIN-GRADOS                             
              PERFORM 24000-CARGO-MINIMA     
           END-IF.                                            
           IF H-TEMPERAT  >= TMAX-GRADOS                      
              PERFORM 25000-CARGO-MAXIMA                      
           END-IF.                                            
                                                              
           PERFORM 27000-CARGO-DETMIN.                        
                                                              
           PERFORM 28000-CARGO-DETMAX.                        
                                                              
           PERFORM 21000-GRABO-LISTADO.                       
                                                              
           PERFORM 23500-FETCH-TREGTEM.                       
                                                              
       21000-GRABO-LISTADO.                                   
      *-------------------                                    
                                                              
           IF WS-LINEAS > WS-LINEAS-MAX                       
              PERFORM 22000-IMPRIMO-TITULOS                   
           END-IF.          
          MOVE      DET01  TO REG-LISTADO.                           
                                                                     
          PERFORM 23000-WRITE-LISTADO.                               
                                                                     
      21200-CONSULTO-TESTATUS.                                       
     *------------------------                                       
                                                                     
          MOVE H-CODDSP                      TO S-CODDISP            
          MOVE H-CODFAB                      TO S-CODFAB             
          MOVE H-CODESTAT                    TO S-CODESTAT           
                                                                     
          EXEC SQL                                                   
               SELECT DESCRIPR INTO :S-DESCRIPR                      
               FROM IDCC22.TESTATUS                                  
               WHERE CODDISP  = :S-CODDISP                           
                 AND CODFAB   = :S-CODFAB                            
                 AND CODESTAT = :S-CODESTAT                          
          END-EXEC.   
                                                                      
          MOVE SQLCODE                       TO WS2-SQLCODE.          
                                                                      
          EVALUATE TRUE                                               
                WHEN DB2-OK                                           
                     MOVE S-DESCRIPR        TO DET01-COD-ESTADO-DESCR 
                     MOVE S-DESCRIPR        TO TOTMAX-COD-ESTADO-DESCR
                     MOVE S-DESCRIPR        TO TOTMIN-COD-ESTADO-DESCR
                WHEN OTHER                                            
                     MOVE 'SIN DESCRIPCION' TO DET01-COD-ESTADO-DESCR 
                     MOVE 'SIN DESCRIPCION' TO TOTMAX-COD-ESTADO-DESCR
                     MOVE 'SIN DESCRIPCION' TO TOTMIN-COD-ESTADO-DESCR
          END-EVALUATE.                                               
                                                                      
      21300-CONSULTO-TFABRICA.                                        
     *------------------------                                        
                                                                      
          MOVE H-CODFAB                      TO F-CODFAB              
                                                                      
          EXEC SQL   
               SELECT NOMBRERE INTO :F-NOMBRERE                      
               FROM IDCC22.TFABRICA                                  
               WHERE CODFAB = :F-CODFAB                              
          END-EXEC.                                                  
                                                                     
          MOVE SQLCODE                       TO WS2-SQLCODE.         
                                                                     
          EVALUATE TRUE                                              
                WHEN DB2-OK                                          
                     MOVE F-NOMBRERE        TO DET01-NOM-FABRICANTE  
                     MOVE F-NOMBRERE        TO TOTMAX-NOM-FABRICANTE 
                     MOVE F-NOMBRERE        TO TOTMIN-NOM-FABRICANTE 
                WHEN OTHER                                           
                     MOVE 'SIN NOMBRE'      TO DET01-NOM-FABRICANTE  
                     MOVE 'SIN NOMBRE'      TO TOTMAX-NOM-FABRICANTE 
                     MOVE 'SIN NOMBRE'      TO TOTMIN-NOM-FABRICANTE 
          END-EVALUATE.                                              
                                                                     
      22000-IMPRIMO-TITULOS.     
      *---------------------                                      
                                                                  
           ADD 1 TO TIT01-PAGINA.                                 
                                                                  
           MOVE TIT01       TO REG-LISTADO.                       
           PERFORM 23000-WRITE-LISTADO.                           
                                                                  
           PERFORM 29000-CARGO-FECHA-HORA.                        
           MOVE TIT02       TO REG-LISTADO.                       
           PERFORM 23000-WRITE-LISTADO.                           
                                                                  
           MOVE TIT03       TO REG-LISTADO.                       
           PERFORM 23000-WRITE-LISTADO.                           
                                                                  
           MOVE TIT05      TO REG-LISTADO.                        
           PERFORM 23000-WRITE-LISTADO.                           
                                                                  
           MOVE TIT06      TO REG-LISTADO.                        
           PERFORM 23000-WRITE-LISTADO.    
                                                                  
          MOVE TIT07      TO REG-LISTADO.                         
          PERFORM 23000-WRITE-LISTADO.                            
                                                                  
          MOVE TIT08      TO REG-LISTADO.                         
          PERFORM 23000-WRITE-LISTADO.                            
                                                                  
     * WRITE LISTADO + AGREGADO DE PAGINACIONACION                
      23000-WRITE-LISTADO.                                        
     *-------------------                                         
          IF WS-LINEAS > WS-LINEAS-MAX                            
             MOVE 0                 TO WS-LINEAS                  
             WRITE REG-LISTADO-FD   FROM REG-LISTADO AFTER PAGE   
          ELSE                                                    
             WRITE REG-LISTADO-FD   FROM REG-LISTADO AFTER 1      
          END-IF.                                                 
                                                                  
          EVALUATE FS-LISTADO                                     
              WHEN '00' 
                   ADD 1           TO WS-GRABADOS-LISTADO         
                                      WS-LINEAS                   
              WHEN OTHER                                          
                   DISPLAY 'ERROR WRITE LISTADO FS: ' FS-LISTADO  
                                                                  
          END-EVALUATE.                                           
                                                                  
      23500-FETCH-TREGTEM.                                        
     *---------------------                                       
                                                                  
          INITIALIZE DCLTREGTEMP.                                 
                                                                  
          EXEC SQL                                                
               FETCH CURSOR-TREGTEMP                              
                     INTO                                         
                          :H-CODDSP                               
                         ,:H-CODFAB                               
                         ,:H-FECTOMA                              
                         ,:H-HORTOMA    
                         ,:H-TEMPERAT                           
                         ,:H-HUMEDAD                            
                         ,:H-LATGRAD                            
                         ,:H-LATMIN                             
                         ,:H-LATSEC                             
                         ,:H-LATHEMIS                           
                         ,:H-LONGRAD                            
                         ,:H-LONMIN                             
                         ,:H-LONSEC                             
                         ,:H-LONHEMIS                           
                         ,:H-CODESTAT                           
            END-EXEC.                                           
                                                                
            MOVE SQLCODE                  TO WS-SQLCODE.        
                                                                
            EVALUATE TRUE                                       
                 WHEN DB2-OK                                    
                      SET 88-FS-OPEN-CUR-SI TO TRUE             
                 WHEN DB2-NOTFND        
                      SET 88-FS-OPEN-CUR-NO TO TRUE              
                      CONTINUE                                   
                 WHEN OTHER                                      
                      DISPLAY                                    
                      'OCURRIO UN ERROR AL ABRIR CURSOR-TREGTEMP'
                      DISPLAY 'SQL-CODE: ' WS-SQLCODE            
            END-EVALUATE.                                        
                                                                 
      24000-CARGO-MINIMA.                                        
     *------------------                                         
          MOVE H-CODESTAT                  TO TMIN-COD-ESTADO.   
          MOVE H-CODDSP                    TO TMIN-COD-DISP.     
          MOVE H-CODFAB                    TO TMIN-COD-FABRI.    
          MOVE H-FECTOMA(1:4)              TO TMIN-FEC-AAAA.     
          MOVE H-FECTOMA(6:2)              TO TMIN-FEC-MM.       
          MOVE H-FECTOMA(9:2)              TO TMIN-FEC-DD.       
          MOVE H-HORTOMA(1:2)              TO TMIN-HORA-HH.      
          MOVE H-HORTOMA(4:2)              TO TMIN-HORA-MM.      
          MOVE H-HORTOMA(7:2)              TO TMIN-HORA-SS.      
          MOVE H-TEMPERAT                  TO TMIN-GRADOS.    
         MOVE H-HUMEDAD                   TO TMIN-HUMEDAD.       
         MOVE H-LATGRAD                   TO TMIN-LAT-GRAD.      
         MOVE H-LATMIN                    TO TMIN-LAT-MIN.       
         MOVE H-LATSEC                    TO TMIN-LAT-SEG.       
         MOVE H-LATHEMIS                  TO TMIN-LAT-HEM.       
         MOVE H-LONGRAD                   TO TMIN-LONG-GRAD.     
         MOVE H-LONMIN                    TO TMIN-LONG-MIN.      
         MOVE H-LONSEC                    TO TMIN-LONG-SEG.      
         MOVE H-LONHEMIS                  TO TMIN-LONG-HEM.      
                                                                 
     25000-CARGO-MAXIMA.                                         
    *------------------                                          
         MOVE H-CODESTAT                  TO TMAX-COD-ESTADO.    
         MOVE H-CODDSP                    TO TMAX-COD-DISP.      
         MOVE H-CODFAB                    TO TMAX-COD-FABRI.     
         MOVE H-FECTOMA(1:4)              TO TMAX-FEC-AAAA.      
         MOVE H-FECTOMA(6:2)              TO TMAX-FEC-MM.        
         MOVE H-FECTOMA(9:2)              TO TMAX-FEC-DD.        
         MOVE H-HORTOMA(1:2)              TO TMAX-HORA-HH.   
          MOVE H-HORTOMA(4:2)              TO TMAX-HORA-MM.     
          MOVE H-HORTOMA(7:2)              TO TMAX-HORA-SS.     
          MOVE H-TEMPERAT                  TO TMAX-GRADOS.      
          MOVE H-HUMEDAD                   TO TMAX-HUMEDAD.     
          MOVE H-LATGRAD                   TO TMAX-LAT-GRAD.    
          MOVE H-LATMIN                    TO TMAX-LAT-MIN.     
          MOVE H-LATSEC                    TO TMAX-LAT-SEG.     
          MOVE H-LATHEMIS                  TO TMAX-LAT-HEM.     
          MOVE H-LONGRAD                   TO TMAX-LONG-GRAD.   
          MOVE H-LONMIN                    TO TMAX-LONG-MIN.    
          MOVE H-LONSEC                    TO TMAX-LONG-SEG.    
          MOVE H-LONHEMIS                  TO TMAX-LONG-HEM.    
                                                                
      26000-CARGO-DET01.                                        
     *-----------------------                                   
          MOVE H-CODDSP                    TO DET01-COD-DISPO.  
          PERFORM 12100-LEO-MAEDISP.                            
          INITIALIZE                       DCLTESTATUS.         
          PERFORM 21200-CONSULTO-TESTATUS. 
          INITIALIZE                       DCLTFABRICA.             
          PERFORM 21300-CONSULTO-TFABRICA.                          
          MOVE H-CODFAB                    TO DET01-COD-FABRICANTE. 
          MOVE H-FECTOMA(1:4)              TO DET01-FECHA-AAAA.     
          MOVE H-FECTOMA(6:2)              TO DET01-FECHA-MM.       
          MOVE H-FECTOMA(9:2)              TO DET01-FECHA-DD.       
          MOVE H-HORTOMA(1:2)              TO DET01-HH.             
          MOVE H-HORTOMA(4:2)              TO DET01-MM.             
          MOVE H-HORTOMA(7:2)              TO DET01-SS.             
          MOVE H-TEMPERAT                  TO DET01-TEMPERATURA.    
          MOVE H-HUMEDAD                   TO DET01-HUMEDAD.        
          MOVE H-LATGRAD                   TO DET01-LAT-GRA.        
          MOVE H-LATMIN                    TO DET01-LAT-MIN.        
          MOVE H-LATSEC                    TO DET01-LAT-SEC.        
          MOVE H-LATHEMIS                  TO DET01-LAT-HEM.        
          MOVE H-LONGRAD                   TO DET01-LONG-GRA.       
          MOVE H-LONMIN                    TO DET01-LONG-MIN.       
          MOVE H-LONSEC                    TO DET01-LONG-SEC.       
          MOVE H-LONHEMIS                  TO DET01-LONG-HEM.    
          MOVE H-CODESTAT                  TO DET01-COD-ESTADO.    
                                                                   
          IF  DET01-LAT-HEM  = 'S'                                 
              MOVE 'SOUTH' TO DET01-LAT-HEMIS-DESC                 
          ELSE IF DET01-LAT-HEM = 'N'                              
              MOVE 'NORTH' TO DET01-LAT-HEMIS-DESC                 
          END-IF.                                                  
                                                                   
          IF  DET01-LONG-HEM = 'E'                                 
              MOVE 'EAST' TO DET01-LONG-HEMIS-DESC                 
          ELSE IF DET01-LONG-HEM = 'W'                             
              MOVE 'WEST' TO DET01-LONG-HEMIS-DESC                 
          END-IF.                                                  
                                                                   
      27000-CARGO-DETMIN.                                          
     *------------------                                           
          MOVE TMIN-COD-ESTADO             TO TOTMIN-COD-ESTADO.   
          PERFORM 12100-LEO-MAEDISP.                               
          INITIALIZE                       DCLTESTATUS.   
           PERFORM 21200-CONSULTO-TESTATUS.                           
           INITIALIZE                       DCLTFABRICA.              
           PERFORM 21300-CONSULTO-TFABRICA.                           
           MOVE TMIN-COD-DISP               TO TOTMIN-COD-DISPO.      
           MOVE TMIN-COD-FABRI              TO TOTMIN-COD-FABRICANTE. 
           MOVE TMIN-FEC-AAAA               TO TOTMIN-FECHA-AAAA.     
           MOVE TMIN-FEC-MM                 TO TOTMIN-FECHA-MM.       
           MOVE TMIN-FEC-DD                 TO TOTMIN-FECHA-DD.       
           MOVE TMIN-HORA-HH                TO TOTMIN-HH.             
           MOVE TMIN-HORA-MM                TO TOTMIN-MM.             
           MOVE TMIN-HORA-SS                TO TOTMIN-SS.             
           MOVE TMIN-GRADOS                 TO TOTMIN-TEMPERATURA.    
           MOVE TMIN-HUMEDAD                TO TOTMIN-HUMEDAD.        
           MOVE TMIN-LAT-GRAD               TO TOTMIN-LAT-GRA.        
           MOVE TMIN-LAT-MIN                TO TOTMIN-LAT-MIN.        
           MOVE TMIN-LAT-SEG                TO TOTMIN-LAT-SEC.        
           MOVE TMIN-LAT-HEM                TO TOTMIN-LAT-HEM.        
           MOVE TMIN-LONG-GRAD              TO TOTMIN-LONG-GRA.       
           MOVE TMIN-LONG-MIN               TO TOTMIN-LONG-MIN.  
          MOVE TMIN-LONG-SEG               TO TOTMIN-LONG-SEC.      
          MOVE TMIN-LONG-HEM               TO TOTMIN-LONG-HEM.      
                                                                    
          IF TOTMIN-LAT-HEM = 'S'                                   
              MOVE 'SOUTH' TO TOTMIN-LAT-HEMIS-DES                  
          ELSE IF TOTMIN-LAT-HEM  = 'N'                             
              MOVE 'NORTH' TO TOTMIN-LAT-HEMIS-DES                  
          END-IF.                                                   
          IF TOTMIN-LONG-HEM = 'E'                                  
              MOVE 'EAST' TO TOTMIN-LONG-HEMIS-DES                  
          ELSE IF TOTMIN-LONG-HEM = 'W'                             
              MOVE 'WEST' TO TOTMIN-LONG-HEMIS-DES                  
          END-IF.                                                   
                                                                    
      28000-CARGO-DETMAX.                                           
     *------------------                                            
          MOVE TMAX-COD-ESTADO             TO TOTMAX-COD-ESTADO.    
          MOVE TMAX-COD-DISP               TO TOTMAX-COD-DISPO.     
          MOVE TMAX-COD-FABRI              TO TOTMAX-COD-FABRICANTE.    
          PERFORM 12100-LEO-MAEDISP.                                
          INITIALIZE                       DCLTESTATUS.             
          PERFORM 21200-CONSULTO-TESTATUS.                          
          INITIALIZE                       DCLTFABRICA.             
          PERFORM 21300-CONSULTO-TFABRICA.                          
          MOVE TMAX-FEC-AAAA               TO TOTMAX-FECHA-AAAA.    
          MOVE TMAX-FEC-MM                 TO TOTMAX-FECHA-MM.      
          MOVE TMAX-FEC-DD                 TO TOTMAX-FECHA-DD.      
          MOVE TMAX-HORA-HH                TO TOTMAX-HH.            
          MOVE TMAX-HORA-MM                TO TOTMAX-MM.            
          MOVE TMAX-HORA-SS                TO TOTMAX-SS.            
          MOVE TMAX-GRADOS                 TO TOTMAX-TEMPERATURA.   
          MOVE TMIN-HUMEDAD                TO TOTMAX-HUMEDADPIC.    
          MOVE TMAX-LAT-GRAD               TO TOTMAX-LAT-GRA.       
          MOVE TMAX-LAT-MIN                TO TOTMAX-LAT-MIN.       
          MOVE TMAX-LAT-SEG                TO TOTMAX-LAT-SEC.       
          MOVE TMAX-LAT-HEM                TO TOTMAX-LAT-HEM.       
          MOVE TMAX-LONG-GRAD              TO TOTMAX-LONG-GRA.      
          MOVE TMAX-LONG-MIN               TO TOTMAX-LONG-MIN.   
          MOVE TMAX-LONG-SEG               TO TOTMAX-LONG-SEC.    
          MOVE TMAX-LONG-HEM               TO TOTMAX-LONG-HEM.    
                                                                  
          IF TOTMAX-LAT-HEM = 'S'                                 
              MOVE 'SOUTH' TO TOTMAX-LAT-HEMIS-DES                
          ELSE IF TOTMAX-LAT-HEM = 'N'                            
              MOVE 'NORTH' TO TOTMAX-LAT-HEMIS-DES                
          END-IF.                                                 
          IF TOTMAX-LONG-HEM = 'E'                                
              MOVE 'EAST' TO TOTMAX-LONG-HEMIS-DES                
          ELSE IF TOTMAX-LONG-HEM = 'W'                           
              MOVE 'WEST' TO TOTMAX-LONG-HEMIS-DES                
          END-IF.                                                 
                                                                  
      29000-CARGO-FECHA-HORA.                                     
     *----------------------                                      
          MOVE FUNCTION CURRENT-DATE       TO WS-CURRENT-DATE.    
          MOVE WS-CURRDATE-AAAA            TO TIT02-FECHA-DD.     
          MOVE WS-CURRDATE-MM              TO TIT02-FECHA-MM.      
          MOVE WS-CURRDATE-DD              TO TIT02-FECHA-AAAA.    
          MOVE WS-CURRDATE-HH              TO TIT02-HORA-HH.       
          MOVE WS-CURRDATE-MN              TO TIT02-HORA-MM.       
          MOVE WS-CURRDATE-SS              TO TIT02-HORA-SS.       
                                                                   
      30000-FINALIZO.                                              
     *--------------                                               
                                                                   
          PERFORM 30100-TOTALES-CONTROL.                           
                                                                   
          PERFORM 31000-CIERRO-ARCHIVOS.                           
                                                                   
                                                                   
      30100-TOTALES-CONTROL.                                       
     *---------------------                                        
                                                                   
          MOVE TIT09      TO REG-LISTADO.                          
          PERFORM 23000-WRITE-LISTADO.                             
                                                                      
          MOVE TOTMIN     TO REG-LISTADO.                             
          PERFORM 23000-WRITE-LISTADO.                                
                                                                      
          MOVE DETMIN     TO REG-LISTADO.                             
          PERFORM 23000-WRITE-LISTADO.                                
                                                                      
          MOVE TIT010     TO REG-LISTADO.                             
          PERFORM 23000-WRITE-LISTADO.                                
                                                                      
          MOVE TOTMAX-01  TO REG-LISTADO.                             
          PERFORM 23000-WRITE-LISTADO.                                
                                                                      
          MOVE DETMAX    TO REG-LISTADO.                              
          PERFORM 23000-WRITE-LISTADO.                                
                                                                      
          MOVE WS-GRABADOS-LISTADO         TO WS-GRABADOS-LISTADO-ED. 
      31000-CIERRO-ARCHIVOS.                                          
     *---------------------    
                                                                       
          IF 88-OPEN-MAEDISP-SI                                        
             SET 88-OPEN-MAEDISP-NO TO TRUE                            
             PERFORM 31130-CIERRO-MAEDISP                              
          END-IF.                                                      
                                                                       
          IF 88-OPEN-LISTADO-SI                                        
             SET 88-OPEN-LISTADO-NO TO TRUE                            
             PERFORM 31120-CIERRO-LISTADO                              
          END-IF.                                                      
                                                                       
          PERFORM 31111-CIERRO-CURSOR-TREGTEMP.                        
                                                                       
                                                                       
      31111-CIERRO-CURSOR-TREGTEMP.                                    
     *-----------------------------                                    
                                                                       
          EXEC SQL                                                     
              CLOSE CURSOR-TREGTEMP     
         END-EXEC.                                                 
                                                                   
         MOVE SQLCODE TO WS-SQLCODE.                               
                                                                   
         EVALUATE TRUE                                             
             WHEN DB2-OK                                           
                  CONTINUE                                         
             WHEN OTHER                                            
                   DISPLAY 'ERROR AL CERRAR CURSOR-TREGTEMP'       
                   DISPLAY 'SQLCODE: ' WS-SQLCODE                  
         END-EVALUATE.                                             
                                                                   
     31130-CIERRO-MAEDISP.                                         
    *---------------------                                         
                                                                   
         CLOSE MAEDISP.                                            
                                                                   
         EVALUATE TRUE                                             
             WHEN 88-FS-MAEDISP-OK    
                   CONTINUE                                        
              WHEN OTHER                                           
                   DISPLAY 'ERROR CLOSE MAEDISP FS: ' FS-MAEDISP   
                   STOP RUN                                        
                                                                   
          END-EVALUATE.                                            
                                                                   
      31120-CIERRO-LISTADO.                                        
     *---------------------                                        
                                                                   
          CLOSE LISTADO.                                           
                                                                   
          EVALUATE TRUE                                            
              WHEN 88-FS-LISTADO-OK                                
                   CONTINUE                                        
              WHEN OTHER                                           
                   DISPLAY 'ERROR CLOSE LISTADO FS: ' FS-LISTADO   
                   STOP RUN   
                                                        
           END-EVALUATE.                                
