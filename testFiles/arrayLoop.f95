program arrayLoop

! module arrayLoop

!  contains
!   subroutine bondv1(a,b,c,d)
!     use common_sn 
!         integer, intent(In) :: a = (/1,2,3,4,5/)
!         integer, intent(In) :: b
!         integer, intent(In) :: c
!         integer, intent(In) :: d

  integer :: a(5) = (/1,2,3,4,5/)


    reduction = 0
    do i = 1, 5
      hello = reduction
      reduction = reduction + a(i) - reduction
!       reduction = this + is + a + test
!       reduction = reduction + a(i)
!       red1 = red0 + red0 + a(1)
!       red2 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2)
!       red3 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3)
!       red4 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3) + a(4)

    end do
    nonTemp = reduction

!     reduction = 0
!     do i = 1, 5
!       reduction = reduction + f(reduction)
! !       reduction = reduction + a(i)
! !       red1 = red0 + a(1)
! !       red2 = red0 + a(1) + a(2)
! !       red3 = red0 + a(1) + a(2) + a(3)
! !       red4 = red0 + a(1) + a(2) + a(3) + a(4)

! !       reduction3 = reduction3 + f(reduction3)
! !       red1 = red0 + f(red0)
! !       red2 = red0 + f(red0) + f(red0 + f(red0))
! !       red3 = red0 + f(red0) + f(red0 + f(red0)) + f(red0 + f(red0) + f(red0 + f(red0)))
! !       red4 = red0 + f(red0) + f(red0 + f(red0)) + f(red0 + f(red0) + f(red0 + f(red0))) + f(red0 + f(red0) + f(red0 + f(red0)) + f(red0 + f(red0) + f(red0 + f(red0))))

!     end do
!     nonTemp = reduction

!   do 15 loop1=0,10
!     a(loop1) = 1 + 2
!   15 continue

!   do 15 loop1=0,10
!     b(loop1) = 3 + 4
!   15 continue

!   do 15 loop1=0,10
!     c(loop1) = 1 + 2
!     d(loop1) = 3 + 4
!   15 continue

!   do 15 outer=0,10
!     do 25 inner=0,10
!       a(outer,inner) = 1 + 2
!     25 continue
!   15 continue


!     return
!       end subroutine bondv1

end program arrayLoop
! end module arrayLoop