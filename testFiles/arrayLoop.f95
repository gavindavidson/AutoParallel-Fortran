program arrayLoop
  
        integer, intent(In) :: a = (/1,2,3,4,5/)
        integer, intent(In) :: b
        integer, intent(In) :: c
        integer, intent(In) :: d

do loop1=0, d
!   do loop2=0, c 
    a(loop1,loop2) = 12+45
    a(loop1,loop2) = 12+45
!   end do
end do

do loop1=0, d
!   do loop2=0, c+1
    b(loop1,loop2) = 12+45
!   end do
end do
! if (1 > 2) then
!   dave = "hello"
! end if 

! do loop1=0, d + 1
!   do loop2=0, c 
!     if (loop1 < d) then
!       a(loop1,loop2) = 12+45
!     endif
!     b(loop1,loop2) = 12+45
!   end do
! end do
!   do 15 loop1=0,d
!     a(loop1) = 1 + 2
!   15 continue

!   do 15 loop1=0,d+1
!     b(loop1) = 3 + 4
!   15 continue

! ! ! BECOMES

!   do 15 loop2=0,d + 1
!     if (loop2 < d) then
!       a(loop2) = 1 + 2
!     endif
!     b(loop2) = 3 + 4
!   15 continue




!   do 15 loop1=0,d + 5
!     a(loop1) = 1 + 2
!   15 continue

!   do 15 loop1=0,d
!     b(loop1) = 3 + 4
!   15 continue

!   do 15 loop1=0,d + 5
!     a(loop1) = 1 + 2
!     if (loop1 < d)
!       b(loop1) = 3 + 4
!     endif
!   15 continue


!   do 15 loop1=0,d - 5
!     a(loop1) = 1 + 2
!   15 continue

!   do 15 loop1=0,d
!     b(loop1) = 3 + 4
!   15 continue

!   do 15 loop1=0,d
!     if (loop1 < d - 5)
!       a(loop1) = 1 + 2
!     endif
!     b(loop1) = 3 + 4
!   15 continue


!   do 15 loop1=0,10
!     c(loop1) = 1 + 2
!     d(loop1) = 3 + 4
!   15 continue

! module arrayLoop

!  contains
!   subroutine bondv1(a,b,c,d)
!     use common_sn 
!         integer, intent(In) :: a = (/1,2,3,4,5/)
!         integer, intent(In) :: b
!         integer, intent(In) :: c
!         integer, intent(In) :: d

!   integer :: a(5) = (/1,2,3,4,5/)

!   do 15 outer=0,10
!     do 25 inner=0,10
!       a(outer,inner) = 1 + 2
!     25 continue
!   15 continue

!     reduction = 0
!     do i = 1, 5
!       hello = reduction
!       reduction = reduction + a(i) - reduction
!       reduction = this + is + a + test
!       reduction = reduction + a(i)
!       red1 = red0 + red0 + a(1)
!       red2 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2)
!       red3 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3)
!       red4 = red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + red0 + red0 + a(1) + red0 + red0 + a(1) + a(2) + a(3) + a(4)

!     end do
!     nonTemp = reduction

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

!     return
!       end subroutine bondv1

end program arrayLoop
! end module arrayLoop