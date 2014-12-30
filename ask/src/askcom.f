C================================================================
      subroutine askcom(prompt, coms, selected, opts, nums, strs)
      implicit none
C================================================================
c... 
c... call command   present the user with a prompt and have them select
c...                a command for the list given.

      character prompt*(*)      ! command prompt
      character coms(*)*(*)     ! syntax for each command (input)
      integer selected          ! returned command (output)
      logical opts(*)           ! returned opts(i) = true if opt specified
      real*8 nums(*)            ! returned numeric args
      character strs(*)*(*)     ! returned string args

      character hlpfil*(*), key*(*) ! parameters to comhelp

      logical eval

      character line(512)*1, sline*512, code*4
      equivalence (line, sline)
      integer pos, begtok, endtok, i, ch, outch, strpos, numpos, cend
      character hlp_fil*80, key_wd*40
      data hlp_fil /' '/, key_wd /'ASKCOMMAND'/
      save hlp_fil, key_wd

 1    selected = -1
      do while (selected .eq. -1)
c...  get command line
         call parse_push_delims(char(0))
         call ask1(prompt, '512S', line)
         call parse_pop_delims

c...  grab the command word off the line
         pos = 1
         call gettoken(line, len(sline), ' /', pos, begtok, endtok)
         call str_upper(sline(begtok:endtok))
c         write (*,*) 'ASKCOM: command [', sline(begtok:endtok), ']'

c...  match command
         i = 1
         do while (coms(i)(4:4).ne.' ')
            cend = endtok-begtok+4
            if (sline(begtok:endtok) .eq.
     $           coms(i)(4:cend)) then
c     test for an exact match
               if (index(' #$',coms(i)(cend+1:cend+1)).ne.0) then
                  selected = i
                  go to 20
               endif
c     if already have a match, then set ambiguous flag
               if (selected .ne. -1) selected = -2
c     if no match, then remember which command matched
               if (selected .eq. -1) selected = i
            end if
            i = i + 1
         end do
 20      continue
         if (selected .eq. -2) then
            selected = -1
            if (begtok .le. endtok)
     $           call tell0('Ambiguous command---use more characters')
         else if (selected .ne. -1) then
            continue
         else if (sline(begtok:endtok) .eq. 'HELP') then
            call help(hlp_fil, key_wd)
         else if (sline(begtok:endtok) .eq. '?') then
            call askcomdisplay(coms)
         else
            write (code,fmt='(i3a1)') endtok-begtok+1, 'S'
            call tell1('Unrecognized command (try ? or help) : ',
     $           code, line(begtok))
         endif
      end do
c
c...  for the moment, assume there are no optional parameters.
      strpos = 1
      numpos = 1
      do i=4,len(coms(selected))
c...     check if we need a string parameter
         if (coms(selected)(i:i) .eq. '$') then
            call gettoken(line, len(sline), ',/', pos, begtok, endtok)
            ch = begtok
            outch = 1
            do while (ch .le. endtok) ! copy string, ''->'
               if (line(ch).eq.'''' .and. line(ch+1).eq.'''') ch=ch+1
               strs(strpos)(outch:outch) = line(ch)
               outch = outch + 1
               ch = ch + 1
            end do
            do outch=outch,len(strs(strpos))
               strs(strpos)(outch:outch) = ' '
            end do
            strpos = strpos+1
c...     check if we need a numeric parameter
         else if (coms(selected)(i:i) .eq. '#') then
            call gettoken(line, len(sline), ',/', pos, begtok, endtok)
            if (endtok - begtok .lt. 0) then
               nums(numpos) = 0.0D0
            else if (.not.eval(sline(begtok:endtok), nums(numpos))) then
               write (code,fmt='(i3a1)') endtok-begtok+1, 'S'
               call tell1('invalid expression: ', code, line(begtok))
               go to 1
            end if
            numpos = numpos + 1
         end if
      end do

c...  translate from selected command number to return code for that command
      read(coms(selected)(1:3),*) selected
      return

C================================================================
      entry askcomhelp (hlpfil, key)
      hlp_fil = hlpfil
      key_wd = key
      return
      end

C================================================================
      subroutine askcomdisplay(coms)
C================================================================
      implicit none
      character coms(*)*(*)     ! syntax for each command (input)

      character line*80
      integer pos, i, k, begtok, endtok, toklen

      pos = 1
      line = ' '
      i = 1
      do while (coms(i)(4:4) .ne. ' ')
         k = 1
         call gettoken(coms(i), len(coms(i)), '#$/', k, begtok, endtok)
         toklen = endtok-begtok+1-3 ! skip the return code
         if (pos + toklen .ge. 79) then
            call tell0(line(1:pos))
            line=' '
            pos = 1
         endif
         line(pos:) = coms(i)(4:endtok)
         do while (toklen .ge. 1)
            pos = pos + 11
            toklen = toklen - 11
         end do
         i = i+1
      end do
      if (pos .gt. 1) call tell0(line(1:pos))
      return
      end
         
