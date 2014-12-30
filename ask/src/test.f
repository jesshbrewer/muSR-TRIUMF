      call testaskif
      call testask
      call testcmli
      end
C =====================================================================
      subroutine testcmli

      INTEGER NCMD, CMD_LEN
      PARAMETER (NCMD=27,CMD_LEN=8)
      
      INTEGER SELECTED
      CHARACTER*20 P(3)
      CHARACTER CMD(NCMD)*(CMD_LEN)
      DATA CMD /'TITLE   ', 'ABSTRACT', 'COMMENTS', 'LABELS  ',
     >		  'ZERO    ', 'DATA    ', 'THEORY  ', 'PLOT    ', 
     >		  'MODIFY  ', 'NORMALIZ', 'GENERATE', 'DELETE  ',
     >		  'SHOW    ', 'WRITE   ', 'PRINT   ', 'XYPLOT  ', 
     >		  'SAVE    ', 'LOAD    ', 'SORT    ', 'PACK    ',
     >		  'WEED    ', 'FIT     ', 'LINE    ', 'CURVE   ', 
     >		  'RENAME  ', '        ', 'EXIT    '/

      selected = 1
      DO while (selected .ne. 27)
         CALL CMLI3 ('TEST>', NCMD, CMD_LEN, CMD, SELECTED, '20S', P(1),
     >        '20S', P(2), '20S', P(3))
         write(*,*) cmd(selected), ':', P(1), ':', P(2), ':', P(3), ':'
      end do
      return
      end
C =====================================================================
      subroutine testaskif
      logical ask_if
      if (ask_if('Do you really want to test this? ')) then
         write (*,*) 'You said YES'
      else
         write (*,*) 'You said NO'
      endif
      end
C =====================================================================
      subroutine testask
      INTEGER*4 N, NCH, I
      REAL*4 R(3)
      CHARACTER*1 S(20)
c
      call ask1('Get int ', 'I', N)
      call tell1('Got int ', 'I', N)
c      call ask1('Get real ', 'R', R)
c      call tell1('Got real ', 'R', R)
c      call ask1('Get string ', '20S', S)
c      call tell1('Got string ', '20S', S)
      call ask3s('Get int, real, string: ','I',N,'R',R,'20S',S,NCH)
      write (*,*) 'I=', N, ' R=', R(1), ' S=', (S(I), I=1,20)
      call tell3('Got int, real, string: ','I',N,'R',R,'20S',S)
      write (*,*) 'Entered: ', NCH
      call ask3('Get int, 3 real, string: ','I',N,'3R',R,'20S',S)
      call tell3('Got int, 3 real, string: ','I',N,'3R',R,'20S',S)
      end
