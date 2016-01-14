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

!    i = g_id/(25*15)
   do 15 i=0,10
!     total = i
!      i = 1
!      j = 7
!      j = 9
      reduce(i) = 0
       do 20 g=0,15
!         arr(i,g) = arr(i,g) + spew(i,g)
        reduce(i,g) = arr(i,g)
        reduce(i,g) = arr(i,g)
!         j = 6
!           j = 7
!           arr(k) = 10
!           arr(k) = spew(k)
!           arr(k) = spew(k)*g
!           arr(123) = 12
!           do 25 j = 0, 230
!             spew(j,i,g) = 12
!           25 continue
      20 continue
    arr(i) = i
!     total = 1
  15 continue
  total = 1
  total = 2

end program arrayLoop
