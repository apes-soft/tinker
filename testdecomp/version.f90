!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine version  --  create version number for file  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "version" checks the name of a file about to be opened; if
!     if "old" status is passed, the name of the highest current
!     version is returned; if "new" status is passed the filename
!     of the next available unused version is generated
!
!
      
subroutine version (filename,status)

      use iounit
      use output

      implicit none

      integer:: i,leng,trimtext
      integer:: thousand,hundred
      integer:: tens,ones
      logical:: exist
      character (len=1), dimension(0:9):: digit
      character (len=3):: status
      character (len=120):: filename
      character (len=120):: oldfile
      character (len=120):: newfile
      data digit  / '0','1','2','3','4','5','6','7','8','9' /


      ! process the filename and status variables

      call lowcase (status)
      leng = trimtext (filename)

     ! no change is needed if the file doesn't exist

      exist = .false.
      if (leng .ne. 0)  inquire (file=filename(1:leng),exist=exist)
      if (.not. exist)  return

      ! set initial values for the current and next versions

      newfile = filename
      oldfile = filename

      ! append an artificial version number to the filename;
      ! currently handles up to 10000 versions of a file

      if (.not. noversion) then
         i = 1
         do while (exist)
            i        = i + 1
            oldfile  = newfile
            thousand = i / 1000
            hundred  = (i - 1000*thousand) / 100
            tens     = (i - 1000*thousand - 100*hundred) / 10
            ones     = i - 1000*thousand - 100*hundred - 10*tens
            if (thousand .ne. 0) then
               newfile = filename(1:leng)//'_'//digit(thousand) &
                            //digit(hundred)//digit(tens)//digit(ones)
            else if (hundred .ne. 0) then
               newfile = filename(1:leng)//'_'//digit(hundred) &
                            //digit(tens)//digit(ones)
            else if (tens .ne. 0) then
               newfile = filename(1:leng)//'_'//digit(tens)//digit(ones)
            else
               newfile = filename(1:leng)//'_'//digit(ones)
            end if
            inquire (file=newfile,exist=exist)
         end do
      end if

      ! set the file name based on the requested status

      if (status .eq. 'old') then
         filename = oldfile
      else if (status .eq. 'new') then
         filename = newfile
         inquire (file=filename,exist=exist)
         if (exist) then
            call nextarg (filename,exist)
            if (exist) then
               inquire (file=filename,exist=exist)
            else
               write(iout,*) ' Version: No File Name for Coordinate ' &
                             //'Output specified.'
               call fatal
            end if
         end if
      end if
      return
      
end subroutine
