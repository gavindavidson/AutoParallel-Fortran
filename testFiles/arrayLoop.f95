module arrayLoop

 contains
  subroutine bondv1(a,b,c,d)
    use common_sn 
        integer, intent(In) :: a
        integer, intent(In) :: b
        integer, intent(In) :: c
        integer, intent(In) :: d


  do 15 loop1=0,10
    a(loop1) = 1 + 2
  15 continue

  do 15 loop1=0,10
    b(loop1) = 3 + 4
  15 continue

  do 15 loop1=0,10
    c(loop1) = 1 + 2
    d(loop1) = 3 + 4
  15 continue

  do 15 outer=0,10
    do 25 inner=0,10
      a(outer,inner) = 1 + 2
    25 continue
  15 continue


    return
      end subroutine bondv1


end module arrayLoop
