IDENTIFICATION DIVISION.                                      
PROGRAM-ID. PROJECT1.                                         
AUTHOR. PATRICK SHEA.                                         
DATE-WRITTEN. 12/30/2021                                      
                                                              
**************************************************************
 A PROGRAM THAT READS DATA FROM A SEQUENTIAL FILE, INTERPRETS 
 DATA THROUGH COBOL THNEN INSERTS UPDATES AND DELETES         
 ENTRIES FROM DB2                                             
**************************************************************
 ENVIRONMENT DIVISION.                                        
                                                              
 INPUT-OUTPUT SECTION.                                        
 FILE-CONTROL.                                                
     SELECT UI-EMPLOYEE-INPUT ASSIGN TO DDEMPDET.             
                                                              
 DATA DIVISION.                                               
 FILE SECTION.                                                
 FD  UI-EMPLOYEE-INPUT                                        
         RECORDING MODE IS F                                  
         DATA RECORD IS UI-EMPLOYEE-DETAILS.                  
                                                              
01  UI-EMPLOYEE-DETAILS.                                      
    05  UI-FLAG      PIC 9(01).                               
    05  FILLER       PIC X(01).  
         05  UI-EMP-ID    PIC 9(06).                
     05  FILLER       PIC X(01).                
     05  UI-EMP-NAME  PIC X(20).                
     05  FILLER       PIC X(01).                
     05  UI-UNIT-ID   PIC 9(01).                
     05  FILLER       PIC X(49).                
                                                
 WORKING-STORAGE SECTION.                       
                                                
       EXEC SQL                                 
         INCLUDE SQLCA                          
       END-EXEC.                                
                                                
       EXEC SQL                                 
         INCLUDE EMPDEPT                        
       END-EXEC.                                
                                                
       EXEC SQL                                 
         INCLUDE DEPTS                          
       END-EXEC.                                
                                                
 01  MISC.                                      
     05 WS-REC-EOF    PIC X(01)  VALUE SPACES.  
     05 WS-FLAG-CHECK PIC X(01).  
      PROCEDURE DIVISION.                      
                                          
 MAIN-CONTROL.                            
     PERFORM A-INITIAL.                   
     PERFORM B-CONTROL.                   
     PERFORM C-END.                       
                                          
  A-INITIAL.                              
      INITIALIZE WS-REC-EOF WS-FLAG-CHECK 
      OPEN INPUT UI-EMPLOYEE-INPUT        
      READ UI-EMPLOYEE-INPUT              
          AT END                          
              MOVE 'Y' TO WS-REC-EOF      
      END-READ.                           
  B-CONTROL.                              
      PERFORM B-READ UNTIL WS-REC-EOF = 'Y
  B-READ.                                 
      MOVE UI-FLAG TO WS-FLAG-CHECK       
      PERFORM B-FLAG-CHECK                
      PERFORM B-SQL-CODE-CHECK            
      READ UI-EMPLOYEE-INPUT              
          AT END                          
              MOVE 'Y' TO WS-REC-EOF      
      END-READ. 
B-FLAG-CHECK.                                
    EVALUATE WS-FLAG-CHECK                   
        WHEN 'I'                             
            MOVE UI-EMP-ID TO EMP-ID         
            MOVE UI-EMP-NAME TO EMP-NAME     
            MOVE UI-UNIT-ID TO UNIT-ID       
             EXEC SQL                        
                 INSERT INTO EMPDEPT VALUES( 
                 :EMP-ID,                    
                 :EMP-NAME,                  
                 :UNIT-ID)                   
             END-EXEC.                       
    EVALUATE WS-FLAG-CHECK                   
        WHEN 'U'                             
             MOVE UI-EMP-ID TO EMP-ID        
             MOVE UI-EMP-NAME TO EMP-NAME    
             MOVE UI-UNIT-ID TO UNIT-ID      
             EXEC SQL                        
                 UPDATE EMPDEPT              
                   SET EMP_ID = :EMP-ID,     
                       EMP_NAME = :EMP-NAME  
                   WHERE UNIT_ID = :UNIT-ID  
             END-EXEC.                       
    EVALUATE WS-FLAG-CHECK                   
        WHEN 'D' 
                          MOVE UI-EMP-ID TO EMP-ID           
                  MOVE UI-EMP-NAME TO EMP-NAME       
                  MOVE UI-UNIT-ID TO UNIT-ID         
                  EXEC SQL                           
                  DELETE FROM EMPDEPT                
                    WHERE UNIT_ID = :UNIT-ID         
                  END-EXEC.                          
    B-SQL-CODE-CHECK.                                
         IF SQLCODE = 0                              
             DISPLAY 'SUCCESS!'                      
                DISPLAY 'EMP ID ' EMP-ID             
                DISPLAY 'EMP NAME ' EMP-NAME         
                DISPLAY 'UNIT ID ' UNIT-ID           
         ELSE                                        
            DISPLAY UI-FLAG ' OPERATION UNSUCCESFUL' 
            DISPLAY 'SQL CODE: ' SQLCODE             
         END-IF.                                     
    C-END.                                           
        CLOSE UI-EMPLOYEE-INPUT.                     
        STOP RUN.                                                                                                                                           