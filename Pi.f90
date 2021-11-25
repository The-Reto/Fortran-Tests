program Pi
        implicit none

        real(16) :: basel, euler, random, integral
        integer :: n

        n = 10000
        write(*,'(F12.8F12.8F12.8F12.8)') basel(n), euler(n), random(n), integral(n)
end program

function basel(n) result(pi)
        integer, intent(in) :: n
        real(16) :: pi
        integer :: i
        pi = 0.d0
        do i = 1,n
                pi = pi + 1.d0/(i**2)
        end do
        pi = sqrt(6.d0 * pi)
end function

function euler(n) result(pi)
        integer, intent(in) :: n
        real(16) :: pi
        integer :: i
        pi = 0.d0
        do i = 1,n,2
                pi = pi + 1.d0 / (2*i - 1) - 1.d0 / (2*i + 1)
        end do
        pi = 4*pi
end function

function random(n) result(pi)
        integer, intent(in) :: n
        real(16) :: pi, point, rand

        pi = 0.d0
        do i = 1,n
                call random_number(point)
                call random_number(rand)
                point = sqrt(point**2 + rand**2)
                if (point <= 1.d0) then
                        pi = pi + 1.d0/n
                endif
       enddo
       pi = 4*pi
end function

function integral(n) result(pi)
        integer, intent(in) :: n
        real(16) :: pi, x
        integer :: i
        pi = 0.d0
        do i = 0,n-1
                x = i*1.d0/(4.d0*n)
                pi = pi + sqrt(x - x**2)
        enddo
        pi = sqrt(3.d0) * 3.d0/4.d0 + 24*pi/(4.d0*n)
end function
