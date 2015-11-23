program continuation

INTEGER :: A

A = 174.5 * Year   &
    + Count / 100

A = 174.5 * Year   &
!  this is a comment line
&	+ Count / 100

A = 174.5 + ThisIsALong&
&VariableName * 123.45

! A = 174.5 + ThisIsALong	&
! &VariableName * 123.45

end program continuation