program HelloWorld
        implicit none;
        
        character(32):: name  !noone has a name longer than 32 characters, right?

        write(*,*) "Enter your Name:"
        read(*,'(a)') name !'(a)' to not read input as one single string, not as a list
        write(*,*) "Hello ", trim(name), "!" !trim to trim of tailing whitespace, cause it's uglf af otherwise
end program
