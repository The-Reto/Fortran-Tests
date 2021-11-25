module fibonacci_mod
  type res	!defining a container type for the recurrsion (see below)
    integer :: a,b
  end type

  contains

  recursive function fib(n) result(r) !returns the n-th and the (n-1)-th fibonacci number this way I don't need to call the function twice per recurrsion
    integer :: n, temp
    type(res) :: r
    if (n < 3) then !for n = 2 
      r%a = 1
      r%b = 1
    else
      r = fib(n-1) ! r%a = (n-1)-th fibonacci r%b = (n2)-th fibonacci
      temp = r%a
      r%a = r%b + temp !n-th fibonacci is (n-1)-th + (n2)-th fibonacci
      r%b = temp
    end if
  end function
end module

program fibonacci
  use fibonacci_mod !using the modlue defined above, with the container type and the function
  implicit none
  integer :: n_max, n
  type(res) :: fib_n
  real(8) :: phi_approx
  real(8), parameter :: phi = (1 + sqrt(5.0))/2 !golden ratio
  
  write(*,*) "Enter n" !letting the user choose up to what fibonacci number to calculate
  read(*,*) n_max
  
  !printing all Fibonacci numbers up no n_max, also print the resulting approximation to the golden ratio, as well as the error
  write(*,'(A4A10A18A9)') "n", "f_n", "Phi (approx)", "Error"
  do n = 1,n_max
    fib_n = fib(n)
    phi_approx = fib_n%a / real(fib_n%b,8)
    write(*,'(I4I10F18.10E15.5)') n, fib_n%a, phi_approx, abs(phi - phi_approx)
  end do
  
  !printing the results in summary
  write(*,*)
  write(*,'(A4I3A)') "The", n_max, "-nd Fibonacci number is:"
  write(*,*) fib_n%a
  write(*,*) "Our approximation for the golden ratio is"
  write(*,*) phi_approx
  write(*,*) "Our error is"
  write(*,*) abs(phi - phi_approx)
end program


