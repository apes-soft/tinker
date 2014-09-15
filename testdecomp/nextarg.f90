!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1996  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ###############################################################
!     ##                                                           ##
!     ##  subroutine nextarg  --  find next command line argument  ##
!     ##                                                           ##
!     ###############################################################
!
!
!     "nextarg" finds the next unused command line argument
!     and returns it in the input character string
!
!

subroutine nextarg (string,exist)

   use argue

   implicit none

   integer:: i,length
   logical:: exist
   character (len=*):: string


   ! initialize the command argument as a blank string
   string = '          '
   exist  = .false.

   ! get the next command line argument and mark it as used
   if (narg .ne. 0) then
      length = min(len(string),len(arg(maxarg)))
      do i = 1, narg
         if (listarg(i)) then
            listarg(i) = .false.
            string     = arg(i)(1:length)
            exist      = .true.
            goto 10
         end if
      end do
10    continue
   end if
   return
      
end subroutine nextarg
