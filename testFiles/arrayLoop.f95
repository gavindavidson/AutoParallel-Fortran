program arrayLoop
!    implicit none
!    integer i
!    integer arr(12)
!    integer spew(10)
!    integer total
!    integer j
!    integer g
!    integer g_id

!    total = 0
!     integer j(10)

        real(kind=4), intent(In) :: dt
        real(kind=4), dimension(0:ip) , intent(In) :: dxs
        real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
        integer, intent(In) :: im
        integer, intent(In) :: jm
        integer, intent(In) :: km
        integer, intent(In) :: n
        real(kind=4), dimension(0:ip+1,-1:thingIDoNotWant+1,0:kp+1) , intent(InOut) :: u
        real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: v
        real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(InOut) :: w
        real(kind=4), dimension(kp+2) , intent(In) :: z2
        real(kind=4) :: u_val

   do 15 i=0,10
      j = 10
      hello = gavin(1,2,3)

  15 continue
  hello = j
!   print *,j


end program arrayLoop
