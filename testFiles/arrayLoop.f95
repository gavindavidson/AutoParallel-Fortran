program arrayLoop
!    implicit none
   integer i
   integer arr(10)
   integer total
   integer j

   total = 0

   do 15 i=0,10
    total = i
  15 continue

  do 20 i=0,10
    arr(i) = i
  20 continue
!    integer j
!    integer nplots
!    integer nspp
!    integer arr(5, 4)

!    nplots = 5
!    nspp = 4

!   do 821 kp = 1, nplots
!     do 822 ksp = 1, nspp
!         arr(kp, ksp) = 0.0
!   822 continue
!   821 continue

!    do 15 j=1,5
!      do 17 i=1,  42
!        print *,i      
!      17 continue
!    15 continue

end program arrayLoop
