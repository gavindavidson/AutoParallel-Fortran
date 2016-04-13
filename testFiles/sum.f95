program sum

    integer, parameter :: size = 81920000
    real :: start, finish
    integer, dimension(size) :: a

    integer :: total

    do i = 1, size
        a(i) = i-1
    end do

    call cpu_time(start)

    total = 0
    do i = 1, size
        total = total + a(i)
    end do
    call cpu_time(finish)

    print *, "values: ", size
    print *, "total: ", total
    print *, "Time: ", finish-start
    print *,"---"

end program sum