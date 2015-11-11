program select

  INTEGER :: Class
  Class = 2

SELECT CASE (Class)
   CASE (1)
      WRITE(*,*)  'Freshman'
   CASE (2)
      WRITE(*,*)  'Sophomore'
   CASE (3)
      WRITE(*,*)  'Junior'
   CASE (4)
      WRITE(*,*)  'Senior'
   CASE DEFAULT
      WRITE(*,*)  "Hmmmm, I don't know"
END SELECT
WRITE(*,*)  'Done'

end program select
