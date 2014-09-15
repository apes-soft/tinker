!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ################################################################
!     ##                                                            ##
!     ##  subroutine getword  --  extract first word from a string  ##
!     ##                                                            ##
!     ################################################################
!
!
!     "getword" searches an input string for the first alphabetic
!     character (A-Z or a-z); the region from this first character
!     to the first blank space or separator is returned as a "word";
!     if the actual word is too long, only the first part is returned
!
!     variables and parameters:
!
!     string    input character string to be searched
!     word      output with the first word in the string
!     next      input with first position of search string;
!                 output with the position following word
!


subroutine getword (string,word,next)

   implicit none

   character (len=*),intent(in):: string
   character (len=*),intent(out):: word
   integer, intent(inout):: next
   integer:: len,length
   integer:: size
   integer:: first,last
   integer:: extent
   integer:: final
   character(len=1):: letter
   character(len=52), parameter:: begset="abcdefghijklmnopqrstuvwxyz"// &
                                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   character(len=1), parameter:: tab       = char(9)
   character(len=1), parameter:: space     = char(32)
   character(len=1), parameter:: comma     = char(44)
   character(len=1), parameter:: colon     = char(58)
   character(len=1), parameter:: semicolon = char(58)
   character(len=5), parameter:: endset = tab//space//comma//colon//semicolon

   length = len(string(next:)) ! Length of input string
   size   = len(word)          ! Length of output word

   ! Search through the string to find:
   first = scan(string(next:),begset)          ! first alphabetic character
   next  = first+scan(string(first:),endset)-1 ! first separator character 
                                               ! after word
   last  = next - 1                            ! last character in word

   ! trim the actual word if it is too long to return
   extent = next - first
   final  = first + size - 1

   ! if string is smaller than the extent of the word
   if (extent .gt. size)  then
      last   = final
      extent = size
   end if

   ! transfer the word into the return string.
   word(1:extent)     = string(first:last)

   ! pad out remainder of the word with blanks if necessary
   if(size.lt.extent) then
      word(extent:size) = ' '
   end if

   ! skip over the next character when it is a separator
   letter = string(next:next)
   if (letter.eq.tab .or. letter.eq.comma .or. &
       letter.eq.colon .or. letter.eq.semicolon) then
       next = next + 1
   end if

   !print *," next = ",next," is `",string(next:next),"`"

   return

end subroutine getword

