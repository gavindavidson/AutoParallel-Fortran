program arrayLoop
!    implicit none
!    integer i
   integer arr(12)
   integer spew(10)
   integer total
   integer j
   integer g
   integer g_id

   total = 0

   do 15 i=0,10
      temp1 = arr(i) + total
      temp2 = temp1 + arr(i) + temp1
      total = temp2 + 1
!        do 20 g=0,15

!       20 continue

  15 continue
  j = total
  print *,total


end program arrayLoop
