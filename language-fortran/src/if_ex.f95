program if_ex

  INTEGER :: x
  x = -1

IF (x > 0) THEN 
WRITE(*,*)  '+'
ELSE IF (x == 0) THEN
   WRITE(*,*)  '0'
ELSE IF (x == -1) THEN
	WRITE(*,*) 'negative wun'
! ELSE
!    WRITE(*,*)  '-'
END IF

end program if_ex