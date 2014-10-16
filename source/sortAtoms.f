!
! Sort the atoms along the carterian direction corresponding to the
! splitting direction. This codes is based on code written by Daniel
! Bal (ddbal1@yahoo.com) at: http://balfortran.org/qsort_optimized.f90
!
! NB Need to get in touch with him if we are to keep and check
! licensing.
!


      ! qsort routine to sort atoms
      recursive subroutine sortAtoms(dir, start, end)

      use atoms

      implicit none

      integer, intent(in):: dir        ! direction to sort in
      integer, intent(in):: start      ! starting index for the array
      integer, intent(in):: end        ! end index for the array
      real (kind=8):: pivot            ! pivot for quicksort
      real (kind=8):: findPivot        ! function to find starting pivot value
      integer:: left, right, midpoint  ! array index tracking
      integer, parameter:: limit=100   ! change to insertion sort if under limit
                                       ! limit >= 72
      type(atomtype):: temp            ! temporary atom for swap

      ! ensure we have more than one atom
      if(end-start.gt.1) then 

        ! do a qsort if above limit else switch to an insertion sort
        if(end-start.gt.limit) then

          ! find a good value for a qsort pivot
          pivot = findPivot(dir,start, end)

          right = end+1   ! scan from right of the array
          left  = start-1 ! scan from left of the array

          ! while our scans do not meet
          do while (left.lt.right)
             ! scan from the right to the left
             right = right-1
             ! scan from the right to find an element less than the pivot
             do while (right.ge.start.and.atom(right)%pos(dir).gt.
     &                pivot)
                right = right-1
             end do
             ! scan from the left
             left = left+1
             ! scan from the left to find an element greater than the pivot
             do while (left.le.end.and.atom(left)%pos(dir).lt.pivot)
                left = left+1
             end do
             ! if two elements found swap them
             if(left.lt.right) then
                temp        = atom(left)
                atom(left)  = atom(right)
                atom(right) = temp
             endif
          end do

          if (left.eq.right) then
             midpoint = left+1
          else
             midpoint = left
          end if

          call sortAtoms(dir,start, midpoint-1)
          call sortAToms(dir,midpoint+1,end)

        else ! do an insertion sort instead

          call insertionSort(dir,start,end)

        endif
      endif

      end subroutine sortAtoms

      subroutine insertionSort(dir,start,end)

      use atoms

      implicit none

      integer, intent(in):: dir, start, end
      integer:: i,j
      type(atomtype):: temp
  
      outter: do i=start+1, end
             j = i-1
             temp = atom(i)
             inner: do
                       if(j.eq.0) exit inner
                       if(atom(j)%pos(dir).le.temp%pos(dir)) exit inner
                       atom(j+1) = atom(j)
                       j = j - 1
              end do inner
              atom(j+1) = temp
      end do outter
  
      end subroutine insertionSort

      function findPivot(dir, left, right)

      use atoms

      implicit none

      integer, intent(in):: dir, left, right
      real (kind=8):: findPivot, temp
      real (kind=8), dimension(9):: sample
      integer:: eighth
      integer, parameter:: divisor=8

      ! assign the sample interval
      eighth = (right-left+1)/divisor

      ! if it divides exactly we will only get a sample of 8
      if(mod(right-left+1,divisor).eq.0) then
        eighth = eighth - 1
      end if

      ! gather the sample
      sample = atom(left:right:eighth)%pos(dir) 
   
      ! Use a Sort Network for N=9, using Batcher's Merge-Exchange.
      if (sample(1).gt.sample(9)) then
         temp      = sample(1)
         sample(1) = sample(9)
         sample(9) = temp
      end if
      if (sample(1).gt.sample(5)) then
         temp      = sample(1)
         sample(1) = sample(5)
         sample(5) = temp
      end if
      if (sample(2).gt.sample(6)) then
         temp      = sample(2)
        sample(2) = sample(6)
        sample(6) = temp
      end if
      if (sample(3).gt.sample(7)) then
         temp      = sample(3)
         sample(3) = sample(7)
         sample(7) = temp 
      end if
      if (sample(4).gt.sample(8)) then
         temp      = sample(4)
         sample(4) = sample(8) 
         sample(8) = temp 
      end if
      if (sample(5).gt.sample(9)) then
         temp      = sample(5)
         sample(5) = sample(9)
         sample(9) = temp
      end if
      if (sample(1).gt.sample(3)) then
         temp      = sample(1)
         sample(1) = sample(3)
         sample(3) = temp
      end if
      if (sample(2).gt.sample(4)) then
         temp      = sample(2)
         sample(2) = sample(4)
         sample(4) = temp
      end if
      if (sample(5).gt.sample(7)) then
         temp      = sample(5)
         sample(5) = sample(7)
         sample(7) = temp
       end if
       if (sample(6).gt.sample(8)) then
          temp      = sample(6)
          sample(6) = sample(8)
          sample(8) = temp
      end if
      if (sample(3).gt.sample(9)) then
         temp      = sample(3)
         sample(3) = sample(9)
         sample(9) = temp
      end if
      if (sample(3).gt.sample(5)) then
         temp      = sample(3)
         sample(3) = sample(5)
         sample(5) = temp
      end if
      if (sample(4).gt.sample(6)) then
         temp      = sample(4)
         sample(4) = sample(6)
         sample(6) = temp
      end if
      if (sample(7).gt.sample(9)) then
         temp      = sample(7)
         sample(7) = sample(9)
         sample(9) = temp
      end if
      if (sample(1).gt.sample(2)) then
          temp      = sample(1)
          sample(1) = sample(2)
          sample(2) = temp
      end if
      if (sample(3).gt.sample(4)) then
          temp      = sample(3)
          sample(3) = sample(4)
          sample(4) = temp
      end if
      if (sample(5).gt.sample(6)) then
          temp      = sample(5)
          sample(5) = sample(6)
          sample(6) = temp
      end if
      if (sample(7).gt.sample(8)) then
          temp      = sample(7)
          sample(7) = sample(8)
          sample(8) = temp
      end if
      if (sample(2).gt.sample(9)) then
          temp      = sample(2)
          sample(2) = sample(9)
          sample(9) = temp
      end if
      if (sample(2).gt.sample(5)) then
          temp      = sample(2)
          sample(2) = sample(5)
          sample(5) = temp
      end if
      if (sample(4).gt.sample(7)) then
          temp      = sample(4)
          sample(4) = sample(7)
          sample(7) = temp
      end if
      !   if (sample(6).gt.sample(9)) then
      !       temp      = sample(6)
      !       sample(6) = sample(9)
      !       sample(9) = temp
      !   end if ! skipped
      !   if (sample(2).gt.sample(3)) then
      !      temp      = sample(2)
      !      sample(2) = sample(3)
      !      sample(3) = temp
      !   end if ! skipped
      if (sample(4).gt.sample(5)) then
         temp      = sample(4)
         sample(4) = sample(5)
         sample(5) = temp
      end if
      !   if (sample(6).gt.sample(7)) then
      !       temp      = sample(6)
      !       sample(6) = sample(7)
      !       sample(7) = temp
      !   end if ! skipped
      !   if (sample(8).gt.sample(9)) then
      !       temp      = sample(8)
      !       sample(8) = sample(9)
      !       sample(9) = temp
      !   end if ! skipped
    
      ! Return the pivot
      findPivot = sample(5)

      end function findPivot
