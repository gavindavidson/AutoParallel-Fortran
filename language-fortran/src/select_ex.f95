program select_ex

  INTEGER :: Class
  INTEGER :: test
  Class = 12

SELECT CASE (Class)
CASE (1)
test = 1
WRITE(*,*)  'Freshman'
CASE (2)
test = 3
WRITE(*,*)  'Sophomore'
CASE (3)
WRITE(*,*)  'Junior'
CASE (4)
WRITE(*,*)  'Senior'
CASE DEFAULT
WRITE(*,*)  "Hmmmm, I don't know" 
END SELECT
WRITE(*,*)  'Done'

end program select_ex
