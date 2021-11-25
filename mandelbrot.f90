program main
	implicit none
	integer, parameter :: nx = 80
	integer, parameter :: ny = 25
	real*8 :: dx = 1.8d0 / nx
	real*8 :: dy = 1.2d0 / ny
	real*8 :: x0 = 0, y0 =0
	integer, parameter :: nmax = 100
	integer :: ix, iy, A(2*nx+1, 2*ny+1)
	complex*8 :: c

	integer :: mandelbrot
	character :: input
      
	do while(.true.)
		do ix = -nx,nx
		  do iy=-ny,ny
		    c = cmplx(x0 + ix*dx,y0 + iy*dy)
		    A(ix+nx+1, iy+ny+1) = mandelbrot(c, nmax)
		  end do
		end do

		call printMandelbrot(2*nx+1, 2*ny+1, A, nmax)

		!options
		write(*,*) "Set options: (i/o/m/r/q/h)"
		read(*,*) input
		select case(input)
			case('i') !zoom in
				dx = dx/2
				dy = dy/2
			case('o') !zoom out
				dx = dx*2
				dy = dy*2
			case('m') !move
				write(*,*) "Choose direction to move: (u/d/r/l/s)"
				read(*,*) input
					select case(input)
						case('u') !move up
							y0 = y0 - 20*dy
						case('d') !move down
							y0 = y0 + 20*dy
						case('r') !move right
							x0 = x0 + 70*dx
						case('l') !move left
							x0 = x0 - 70*dx
						case('s') !set position
							write(*,*) "Enter x0 y0"
							read(*,*) x0, y0
						case default
							write(*,*) "Invalid option - not moving"
							read(*,*)
					end select
			case('r') !reset
				dx = 1.8d0 / nx
				dy = 1.2d0 / ny
				x0 = 0
				y0 = 0
			case('q') !quit
				exit
			case('h') !help
				write(*,*) "Type:"
				write(*,*) "  i - to zoom in"
				write(*,*) "  o - to zoom out"
				write(*,*) "  m - to move (select direction in second step)" 
				write(*,*) "      u - move up"
				write(*,*) "      d - move down"
				write(*,*) "      l - move left"
				write(*,*) "      r - move right"
				write(*,*) "      s - set position"
				write(*,*) "  r - to reset zoom and position"
				write(*,*) "  q - to quit"
				write(*,*) "  h - to display this help page"
				write(*,*) "PRESS ENTER TO CONTINUE"
				read(*,*)
			case default
				write(*,*) "Invalid option, type 'h' for help." 
		end select
	end do
end program 


function mandelbrot(c, nmax) result(n)
  implicit none
  complex*8, intent(in) :: c
  integer, intent(in) :: nmax
  complex*8 :: z  
  integer :: n
  n = 0
  z = c
  do while (n < nmax .and. abs(z) < 2) !if either is false we quit
    z = z**2 + c
    n = n + 1
  end do
end function

subroutine printMandelbrot(nx, ny, A, nmax)
  implicit none
  integer, intent(in) :: nx, ny, A(nx, ny), nmax
  character(len=nx) :: txt
  integer :: ix, iy

  do iy=1,ny
    do ix=1,nx
      if (A(ix, iy) == nmax) then
        txt(ix:ix+1) = '#'
      else
        if (A(ix,iy) > nmax / 20) then
          txt(ix:ix+1) = '.'
        else
          txt(ix:ix+1) = ' '
        end if
      end if
    end do
    write(*,*) txt
  end do
end subroutine
