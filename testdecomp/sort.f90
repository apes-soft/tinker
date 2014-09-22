!
!
!     ###################################################
!     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
!     ##              All Rights Reserved              ##
!     ###################################################
!
!     #########################################################
!     ##                                                     ##
!     ##  subroutine sort  --  heapsort of an integer array  ##
!     ##                                                     ##
!     #########################################################
!
!
!     "sort" takes an input list of integers and sorts it
!     into ascending order using the Heapsort algorithm
!
!


subroutine sort (n,list)

  implicit none

  integer:: i,j,k,n
  integer:: index,lists
  integer, dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return

end subroutine sort

!
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine sort2  --  heapsort of real array with keys  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "sort2" takes an input list of reals and sorts it
!     into ascending order using the Heapsort algorithm;
!     it also returns a key into the original ordering
!
!

subroutine sort2 (n,list,key)

  implicit none

  integer:: i,j,k,n
  integer:: index,keys
  integer, dimension(*):: key
  real (kind=8):: lists
  real (kind=8), dimension(:):: list


  ! initialize index into the original ordering

  do i = 1, n
     key(i) = i
  end do

  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
        keys = key(k)
     else
        lists = list(index)
        keys = key(index)
        list(index) = list(1)
        key(index) = key(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           key(1) = keys
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           key(i) = key(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
     key(i) = keys
  end do
  return

end subroutine sort2

!
!
!     #################################################################
!     ##                                                             ##
!     ##  subroutine sort3  --  heapsort of integer array with keys  ##
!     ##                                                             ##
!     #################################################################
!
!
!     "sort3" takes an input list of integers and sorts it
!     into ascending order using the Heapsort algorithm;
!     it also returns a key into the original ordering
!
!
      
subroutine sort3 (n,list,key)

  implicit none

  integer:: i,j,k,n
  integer:: index
  integer:: lists
  integer:: keys
  integer, dimension(:):: list
  integer, dimension(:):: key

  ! initialize index into the original ordering

  do i = 1, n
     key(i) = i
  end do

  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
        keys = key(k)
     else
        lists = list(index)
        keys = key(index)
        list(index) = list(1)
        key(index) = key(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           key(1) = keys
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           key(i) = key(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
     key(i) = keys
  end do
  return
      
end subroutine sort3

!
!
!     #################################################################
!     ##                                                             ##
!     ##  subroutine sort4  --  heapsort of integer absolute values  ##
!     ##                                                             ##
!     #################################################################
!
!
!     "sort4" takes an input list of integers and sorts it into
!     ascending absolute value using the Heapsort algorithm
!
!
      
subroutine sort4 (n,list)

  integer:: i,j,k,n
  integer:: index
  integer:: lists
  integer, dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (abs(list(j)) .lt. abs(list(j+1)))  j = j + 1
        end if
        if (abs(lists) .lt. abs(list(j))) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort4

!
!
!     ################################################################
!     ##                                                            ##
!     ##  subroutine sort5  --  heapsort of integer array modulo m  ##
!     ##                                                            ##
!     ################################################################
!
!
!     "sort5" takes an input list of integers and sorts it
!     into ascending order based on each value modulo "m"
!
!
      
subroutine sort5 (n,list,m)

  implicit none

  integer:: i,j,k,m,n
  integer:: index,smod
  integer:: jmod,j1mod
  integer:: lists
  integer, dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           jmod = mod(list(j),m)
           j1mod = mod(list(j+1),m)
           if (jmod .lt. j1mod) then
              j = j + 1
           else if (jmod.eq.j1mod .and. list(j).lt.list(j+1)) then
              j = j + 1
           end if
        end if
        smod = mod(lists,m)
        jmod = mod(list(j),m)
        if (smod .lt. jmod) then
           list(i) = list(j)
           i = j
           j = j + j
        else if (smod.eq.jmod .and. lists.lt.list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort5

!
!
!     #############################################################
!     ##                                                         ##
!     ##  subroutine sort6  --  heapsort of a text string array  ##
!     ##                                                         ##
!     #############################################################
!
!
!     "sort6" takes an input list of character strings and sorts
!     it into alphabetical order using the Heapsort algorithm
!
!
      
subroutine sort6 (n,list)

  implicit none

  integer:: i,j,k,n
  integer:: index
  character (len=256):: lists
  character (len=*), dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort6

!
!
!     ################################################################
!     ##                                                            ##
!     ##  subroutine sort7  --  heapsort of text strings with keys  ##
!     ##                                                            ##
!     ################################################################
!
!
!     "sort7" takes an input list of character strings and sorts it
!     into alphabetical order using the Heapsort algorithm; it also
!     returns a key into the original ordering
!
!
      
subroutine sort7 (n,list,key)

  implicit none

  integer:: i,j,k,n
  integer:: index
  integer:: keys
  integer, dimension(:):: key
  character (len=256):: lists
  character (len=*),dimension(:):: list


  ! initialize index into the original ordering

  do i = 1, n
     key(i) = i
  end do

  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
        keys = key(k)
     else
        lists = list(index)
        keys = key(index)
        list(index) = list(1)
        key(index) = key(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists
           key(1) = keys
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           key(i) = key(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
     key(i) = keys
  end do
  return
      
end subroutine sort7

!
!
!     #########################################################
!     ##                                                     ##
!     ##  subroutine sort8  --  heapsort to unique integers  ##
!     ##                                                     ##
!     #########################################################
!
!
!     "sort8" takes an input list of integers and sorts it into
!     ascending order using the Heapsort algorithm, duplicate
!     values are removed from the final sorted list
!
!
      
subroutine sort8 (n,list)


  implicit none

  integer:: i,j,k,n
  integer:: index
  integer:: lists
  integer, dimension(:):: list(*)


  ! perform the heapsort of the input list
  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists

           ! remove duplicate values from final list
           j = 1
           do i = 2, n
              if (list(i-1) .ne. list(i)) then
                 j = j + 1
                 list(j) = list(i)
              end if
           end do
           if (j .lt. n)  n = j
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort8

!
!
!     ############################################################
!     ##                                                        ##
!     ##  subroutine sort9  --  heapsort to unique real values  ##
!     ##                                                        ##
!     ############################################################
!
!
!     "sort9" takes an input list of reals and sorts it into
!     ascending order using the Heapsort algorithm, duplicate
!     values are removed from the final sorted list
!
!
subroutine sort9 (n,list)

  implicit none

  integer:: i,j,k,n
  integer:: index
  real (kind=8):: lists
  real (kind=8), dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists

           ! remove duplicate values from final list

           j = 1
           do i = 2, n
              if (list(i-1) .ne. list(i)) then
                 j = j + 1
                 list(j) = list(i)
              end if
           end do
           if (j .lt. n)  n = j
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort9

!
!
!     ##############################################################
!     ##                                                          ##
!     ##  subroutine sort10  --  heapsort to unique text strings  ##
!     ##                                                          ##
!     ##############################################################
!
!
!     "sort10" takes an input list of character strings and sorts
!     it into alphabetical order using the Heapsort algorithm,
!     duplicate values are removed from the final sorted list
!
!
      
subroutine sort10 (n,list)

  implicit none

  integer:: i,j,k,n
  integer:: index
  character (len=256):: lists
  character (len=*), dimension(:):: list


  ! perform the heapsort of the input list

  k = n/2 + 1
  index = n
  do while (n .gt. 1)
     if (k .gt. 1) then
        k = k - 1
        lists = list(k)
     else
        lists = list(index)
        list(index) = list(1)
        index = index - 1
        if (index .le. 1) then
           list(1) = lists

           ! remove duplicate values from final list

           j = 1
           do i = 2, n
              if (list(i-1) .ne. list(i)) then
                 j = j + 1
                 list(j) = list(i)
              end if
           end do
           if (j .lt. n)  n = j
           return
        end if
     end if
     i = k
     j = k + k
     do while (j .le. index)
        if (j .lt. index) then
           if (list(j) .lt. list(j+1))  j = j + 1
        end if
        if (lists .lt. list(j)) then
           list(i) = list(j)
           i = j
           j = j + j
        else
           j = index + 1
        end if
     end do
     list(i) = lists
  end do
  return
      
end subroutine sort10

