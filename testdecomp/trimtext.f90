!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1991  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!



!     ############################################################
!     ##                                                        ##
!     ##  function trimtext  --  find last non-blank character  ##
!     ##                                                        ##
!     ############################################################
!
!
!     "trimtext" finds and returns the location of the last
!     non-blank character before the first null character in
!     an input text string; the function returns zero if no
!     such character is found
!
!
      function trimtext (string)
      implicit none
      integer:: i,size,last
      integer:: len,trimtext
      character(len=1):: char
      character(len=1), parameter:: null = char(0)
      character*(*) string

      ! move forward through the string, one character
      ! at a time, looking for first null character

      trimtext = 0
      size = len(string)

      last = size
      do i = 1, size
         if (string(i:i) .eq. null) then
            last = i - 1
            goto 10
         end if
      end do
   10 continue

      ! move backward through the string, one character
      ! at a time, looking for first non-blank character

      do i = last, 1, -1
         if (string(i:i) .gt. ' ') then
            trimtext = i
            goto 20
         end if
      end do
   20 continue
      return
      end
!
!
!     #################################################################
!     ##                                                             ##
!     ##  subroutine justify  --  convert string to right justified  ##
!     ##                                                             ##
!     #################################################################
!
!
!     "justify" converts a text string to right justified format
!     with leading blank spaces
!
!
      subroutine justify (string)
      implicit none
      integer i,k,len
      integer size,last
      character*1 char
      character*1 null
      character*1 letter
      character*(*) string
!
!
!     move backward through the string, one character
!     at a time, looking for first non-blank character
!
      size = len(string)
      null = char(0)
      last = 0
      do i = size, 1, -1
         letter = string(i:i)
         if (letter.ne.' ' .and. letter.ne.null) then
            last = i
            goto 10
         end if
      end do
   10 continue
!
!     move string to the right and pad with leading blanks
!
      do i = last, 1, -1
         k = i + size - last
         string(k:k) = string(i:i)
      end do
      do i = 1, size-last
         string(i:i) = ' '
      end do
      return
      end
!
!
!     ###############################################################
!     ##                                                           ##
!     ##  subroutine upcase  --  convert string to all upper case  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     "upcase" converts a text string to all upper case letters
!
!
      subroutine upcase (string)
      implicit none
      integer:: i,size,len
      integer:: code,ichar
      character(len=1):: char
      character(len=1):: letter
      character*(*) string

      ! convert lower case to upper case one letter at a time

      size = len(string)
      do i = 1, size
         letter = string(i:i)
         code = ichar(letter)
         if (letter.ge.'a' .and. letter.le.'z') &
            string(i:i) = char(code-32)
      end do
      return
      end
!
!
!     ################################################################
!     ##                                                            ##
!     ##  subroutine lowcase  --  convert string to all lower case  ##
!     ##                                                            ##
!     ################################################################
!
!
!     "lowcase" converts a text string to all lower case letters
!
!
      subroutine lowcase (string)
      implicit none
      integer:: i,size
      integer:: code,ichar
      character(len=1):: char
      character(len=1):: letter
      character*(*) string

      ! convert upper case to lower case one letter at a time

      size = len(string)
      do i = 1, size
         letter = string(i:i)
         code = ichar(letter)
         if (letter.ge.'A' .and. letter.le.'Z') &
            string(i:i) = char(code+32)
      end do
      return
      end



