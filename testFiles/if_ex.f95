program if_ex

  INTEGER :: x
  INTEGER :: y
  INTEGER :: gg
  x = -1

IF (x > 0) THEN 
	y = 1
ELSE IF (x == 0) THEN
   y = 2 
ELSE IF (x == -1) THEN
	WRITE(*,*) 'negative wun'
ELSE
   WRITE(*,*)  '-'
END IF

end program if_ex