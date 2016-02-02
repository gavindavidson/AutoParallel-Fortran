module arrayLoop

 contains
  subroutine bondv1(a,b,c,d)
    use common_sn 
        integer, intent(In) :: a
        integer, intent(In) :: b
        integer, intent(In) :: c
        integer, intent(In) :: d


  do 15 loop1=0,10
    a(loop1) = b(loop1) + c(loop1)
  15 continue

  do 15 loop1=0,10
    d(loop1) = b(loop1) - c(loop1)
  15 continue

    return
      end subroutine bondv1


end module arrayLoop
