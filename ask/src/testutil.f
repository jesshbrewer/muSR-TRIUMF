      call testask
      call testaskif
      call testcmli
      call testcommand
      end
C =====================================================================
      subroutine testcommand
      implicit none

      logical opts(1)
      real*8 nums(10)
      character*80 strs(3)
      integer selected

      character*14 cmd(6)
      data cmd /'  5PGENV######','  4PGBOX$##$##','  1PGLAB$$$',
     $     '  2pgupdt','  3EXIT','    '/

c      write(*,*) cmd(1)
      call tell0('Choose a command from the following:')
      call askcomdisplay(cmd)
 1    call askcom('pgplot> ', cmd, selected, opts, nums, strs)
      go to (100, 200, 300, 400, 500), selected
 500  call tell6('pgenv ','D',nums(1),'D', nums(2),'D', nums(3),'D',
     $     nums(4),'I', nint(nums(5)),'I', nint(nums(6)))
      go to 1
 400  call tell6('pgbox ','80S',strs(1),'D',nums(1),'I', nint(nums(2)),
     $     '80S',strs(2),'D', nums(3),'I',nint(nums(4)))
      go to 1
 100  call tell3('pglab ','80S',strs(1),'80S',strs(2),'80S',strs(3))
      go to 1
 200  call tell0('pgupdt')
      go to 1
 300  call tell0('exit')
      return
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
      INTEGER*4 N
      REAL*4 R(3)
      CHARACTER*1 S(20)
      INTEGER*4 NCH, I
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
