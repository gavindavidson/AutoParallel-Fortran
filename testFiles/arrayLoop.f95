module arrayLoop

 contains
  subroutine bondv1(jm,u,z2,dzn,v,w,km,n,im,dt,dxs)
    use common_sn 
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

  reduction = 0
   do 15 i=0,10
      temp = minFun(u(i), interest, interest)
      reduction = interest
      reduction = temp12
  15 continue
    j = reduction
!   print *,j

    return
      end subroutine bondv1


end module arrayLoop
