!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1995  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine command  --  get any command line arguments  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "command" uses the standard Unix-like iargc/getarg routines
!     to get the number and values of arguments specified on the
!     command line at program runtime
!
!

subroutine command

  use argue

  implicit none

  integer:: i,iargc
  character (len=1):: letter
  character (len=20):: blank


  ! initialize command line arguments as blank strings
  narg  = 0
  blank = '                    '
  arg   = blank//blank//blank

  ! get the number of arguments and store each in a string
  narg = iargc ()
  if (narg .gt. maxarg)  narg = maxarg
  do i = 0, narg
     call getarg (i,arg(i))
  end do

  ! mark the command line options as unuseable for input
  listarg(0)      = .false.  ! Name of the program, do not want
  listarg(1:narg) = .true.

  do i = 1, narg
     letter = arg(i)(1:1)
     if (letter .eq. '-') then
        letter = arg(i)(2:2)
        call upcase (letter)
        if (letter.ge.'A' .and. letter.le.'Z') then
           listarg(i)   = .false.
           listarg(i+1) = .false.
        end if
     end if
  end do
  return
      
end subroutine command
