	  PROGRAM GRID
C places 100 molecules in a 20x20 grid and counts's 
C isolated molecules
C
C Written by Mick Scanlon - sept 2020
      REAL G(20,20)
	  INTEGER TIME
	  DO 5 I=1,5
      CALL INIT(G,20)
CDEBUG	  CALL DISPLAY(G,20)
	  CALL MOLPUT(G,20)
CDEBUG      CALL DISPLAY(G,20)
	  CALL CALCNEIGHBORS(G,20)
5     CONTINUE
      END
C
      SUBROUTINE INIT(A,N)
      REAL A(N,N)
      DO 10 I=1,N
      DO 10 J=1,N
      A(I,J)=0.0
10    CONTINUE
      RETURN
      END
C	  
	  SUBROUTINE MOLPUT(A,N)
	  REAL A(N,N)
	  DO 20 J=1,50
15	  I = RANDOMIZE()
	  X = RANDOMIZE()
CDEBUG	  PRINT *, I
	  IF (A(I,X) .NE. 0.0) GOTO 15
	  A(I,X) = 1.0
CDEBUG	  PRINT *, J 
20	  CONTINUE
	  END
C	  
	  SUBROUTINE DISPLAY(A,N)
      REAL A(N,N)
      DO 7 J=1,N
      WRITE (*,30) (A(I,J),I=1,N)
30    FORMAT(50F6.2)
7     CONTINUE
      PRINT *,' '
      RETURN
      END
C Finds a number between 1 and 20	
 		
	  FUNCTION RANDOMIZE() RESULT(Y)
	  X = RAND(0)
	  Y = INT(20*X)+1
CDEBUG	  PRINT *, Y
	  RETURN
	  END
	  
	  SUBROUTINE CALCNEIGHBORS(A,N)
	  REAL A(N,N)
	  INTEGER TOTAL,TOTAL1
	  TOTAL = 0
	  TOTAL1 = 0
	  DO 40 I=1,N
	  DO 40 J=1,N
	  IF (A(I,J) .EQ. 0.0) GOTO 40
	  IF (A(I+1,J) .EQ. 1.0) GOTO 40
	  IF (A(I,J+1) .EQ. 1.0) GOTO 40
	  IF (A(I,J-1) .EQ. 1.0) GOTO 40
	  IF (A(I-1,J) .EQ. 1.0) GOTO 40
	  TOTAL = TOTAL+1
40	  TOTAL1 = TOTAL1+1
	  CONTINUE
	  PRINT *,'ISOLATED MOLECULES OUT OF 100:'
	  PRINT *, TOTAL
CDEBUG	  PRINT *, TOTAL1
	  RETURN
	  END