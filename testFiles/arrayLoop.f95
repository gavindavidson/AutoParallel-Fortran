program arrayLoop
!    implicit none
!    integer i
   integer arr(12)
   integer spew(10)
   integer total
   integer j
   integer g

   total = 0

   do 15 i=0,10
!     total = i
!      i = 1
!      j = 7
!      j = 9
       do 20 g=0,10
        arr(g, i) = spew(g)
!         j = 6
!           j = 7
!           arr(k) = 10
!           arr(k) = spew(k)
!           arr(k) = spew(k)*g
!           arr(123) = 12
      20 continue
!     arr(i) = i
    j = 1
  15 continue

end program arrayLoop
