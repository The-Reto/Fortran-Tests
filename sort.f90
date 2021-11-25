program sort
    implicit none;
    integer, parameter :: arrLen = 50000;
    real(8), allocatable, dimension(:) :: template, slowInsA, fastInsA;
    integer :: arrlslow, arrlenfast, i;
    
    interface
        subroutine naive_Insertion_Sort(arr, arrl, reps)
            implicit none;
            integer :: arrl, i, reps;
            real(8), allocatable, dimension(:) :: arr;
            real(8) :: ins;
            integer :: pos, bisect;
        end subroutine naive_Insertion_Sort
        
        subroutine smart_Insertion_Sort(arr, arrl, reps)
            implicit none;
            integer :: arrl,l,m,i,reps;
            real(8), allocatable, dimension(:) :: arr, valindex;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:,:) :: table;
            real(8) :: ins;
        end subroutine smart_Insertion_Sort
    end interface
    
    allocate(template(arrLen), slowInsA(arrLen), fastInsA(arrLen));
    call random_number(template);
    call quicksort(template, 1, arrLen);
    
    slowInsA = template;
    fastInsA = template;
    
    write(*,*) "Inserting 50000 objects the trivial way..."
    arrlslow = arrLen;
    call naive_Insertion_Sort(slowInsA, arrlslow, arrLen);
    write(*,*) "Finished!"
    write(*,*) "Inserting 100000 objects the non-trivial way..."
    arrlenfast = arrLen;
    call smart_Insertion_Sort(fastInsA, arrlenfast, 2*arrLen);
    write(*,*) "Finished! (This should have been faster, despite inserting double the amount of numbers)"
end program

subroutine naive_Insertion_Sort(arr, arrl, reps)
    implicit none;
    integer :: arrl, i, reps;
    real(8), allocatable, dimension(:) :: arr;
    real(8) :: ins;
    integer :: pos, bisect;
    
    interface
        subroutine insert1d(ins, pos, arr, arrL)
            integer :: pos, arrL, i;
            real(8), allocatable, dimension(:) :: arr, temp;
            real(8) :: ins;
        end subroutine insert1d
    end interface
    
    do i = 1,reps
        call random_number(ins);
        pos = bisect(arr, arrl, ins);
        call insert1d(ins, pos, arr, arrl);
    enddo
end subroutine naive_Insertion_Sort

subroutine insert1d(ins, pos, arr, arrL)
    implicit none;
    integer :: pos, arrL, i;
    real(8), allocatable, dimension(:) :: arr, temp;
    real(8) :: ins;
    allocate(temp(arrL));
    if (ins .ge. arr(arrL)) pos = pos+1;
    temp = arr;
    if (allocated(arr)) deallocate(arr);
    arrL = arrL + 1;
    allocate(arr(arrL));
    do i = 1,arrL;
        if (i < pos) arr(i) = temp(i);
        if (i == pos) arr(i) = ins;
        if (i > pos) arr(i) = temp(i-1);
    enddo
    deallocate(temp);
end subroutine insert1d

subroutine smart_Insertion_Sort(arr, arrl,reps)
    implicit none;
    integer :: arrl,l,m,i,reps;
    real(8), allocatable, dimension(:) :: arr, valindex;
    integer, allocatable, dimension(:) :: ncolelements;
    real(8), allocatable, dimension(:,:) :: table;
    real(8) :: ins;
    
    interface
        subroutine to2d(arr, arrl, table, l, m, ncolelements, valindex)
            implicit none;
            integer :: arrl, l,m;
            real(8), dimension(arrl) :: arr;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:) :: valindex;
            real(8), allocatable, dimension(:,:) :: table;
            integer :: perColumn,i,j,k,reached;
        end subroutine to2d
        
        subroutine insert2d(l,m,ncolelements,table,valindex,ins)
            implicit none;
            integer :: l,m;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:) :: valindex, temparr;
            real(8), allocatable, dimension(:,:) :: table;
            real(8) :: ins, prev, temp;
            integer :: column, bisect, row, i, n;
        end subroutine insert2d
        
        subroutine to1d(table, l,m, ncolelements, arr, arrl)
            implicit none;
            integer :: l,m, arrl;
            real(8), allocatable, dimension(:,:) :: table;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:) :: arr;
            integer :: i,j,k;
        end subroutine to1d
    end interface
    
    call to2d(arr, arrl, table, l, m, ncolelements, valindex);
    do i = 1, reps
        call random_number(ins);
        call insert2d(l,m,ncolelements,table,valindex,ins);
    enddo
    call to1d(table,l,m,ncolelements,arr,arrl);
    
end subroutine smart_Insertion_Sort

subroutine to1d(table, l,m, ncolelements, arr, arrl)
    implicit none;
    integer :: l,m, arrl;
    real(8), allocatable, dimension(:,:) :: table;
    integer, allocatable, dimension(:) :: ncolelements;
    real(8), allocatable, dimension(:) :: arr;
    integer :: i,j,k;
    
    arrl = sum(ncolelements);
    if (allocated(arr)) deallocate(arr);
    allocate(arr(arrl));
    k = 1;
    do i = 1,m
        do j = 1, ncolelements(i)
            arr(k) = table(i,j);
            k = k+1;
        enddo
    enddo
end subroutine to1d

recursive subroutine insert2d(l,m,ncolelements,table,valindex,ins)
    implicit none;
    integer :: l,m;
    integer, allocatable, dimension(:) :: ncolelements;
    real(8), allocatable, dimension(:) :: valindex, temparr;
    real(8), allocatable, dimension(:,:) :: table;
    real(8) :: ins, prev, temp;
    integer :: column, bisect, row, i, n;
    interface
        subroutine to2d(arr, arrl, table, l, m, ncolelements, valindex)
            implicit none;
            integer :: arrl, l,m;
            real(8), dimension(arrl) :: arr;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:) :: valindex;
            real(8), allocatable, dimension(:,:) :: table;
            integer :: perColumn,i,j,k,reached;
        end subroutine to2d
    
        subroutine to1d(table, l,m, ncolelements, arr, arrl)
            implicit none;
            integer :: l,m, arrl;
            real(8), allocatable, dimension(:,:) :: table;
            integer, allocatable, dimension(:) :: ncolelements;
            real(8), allocatable, dimension(:) :: arr;
            integer :: i,j,k;
        end subroutine to1d
    end interface
    column = bisect(valindex,m,ins);
    if (ncolelements(column) == l) then
!        write(*,*) "Had to extend Table"
        call to1d(table, l,m,ncolelements, temparr, n);
        call to2d(temparr, n, table, l,m,ncolelements,valindex);
!        write(*,*) "New Size: l = ", l, " || m = ", m;
        call insert2d(l,m,ncolelements,table,valindex,ins);
    else
        ncolelements(column) = ncolelements(column) + 1;
        row = bisect(table(column,:), ncolelements(column), ins);
        prev = ins;
        do i = row,ncolelements(column)
            temp = table(column,i);
            table(column,i) = prev;
            prev = temp;
        enddo
    endif
end subroutine insert2d

subroutine to2d(arr, arrl, table, l, m, ncolelements, valindex)
    implicit none;
    integer :: arrl, l,m;
    real(8), dimension(arrl) :: arr;
    integer, allocatable, dimension(:) :: ncolelements;
    real(8), allocatable, dimension(:) :: valindex;
    real(8), allocatable, dimension(:,:) :: table;
    integer :: perColumn, i,j,k, reached;
    
    m = int(sqrt(real(arrl))) + 1;
    l = int(m*2.d0);
    if (allocated(table)) deallocate(table);
    if (allocated(ncolelements)) deallocate(ncolelements);
    if (allocated(valindex)) deallocate(valindex);
    allocate(table(m,l), ncolelements(m), valindex(m));
    table = 0.d0;
    ncolelements = 0;
    valindex = 0.d0;
    
    perColumn = arrL / m;
    do i = 1,m-(arrL - perColumn*m)
        do j = 1,perColumn
            table(i,j) = arr((i-1)*perColumn + j);
        enddo
        ncolelements(i) = perColumn;
        valindex(i) = table(i,perColumn);
    enddo
    reached = i;
    k = sum(ncolelements);
    do i = reached, m
        do j = 1,perColumn+1
            k = k+1;
            table(i,j) = arr(k);
        enddo
        ncolelements(i) = perColumn+1;
        valindex(i) = table(i,perColumn+1);
    enddo
end subroutine to2d

function bisect(arr, arrl, ins)
    implicit none;
    integer :: arrl;
    real(8), dimension(arrl) :: arr;
    real(8) :: ins;
    integer :: bisect, step;
    if (arrl > 4) then
        bisect = arrl/2;
        step = arrl/4;
        do while(bisect > 1 .and. bisect < arrL .and..not.(arr(bisect-1) < ins .and. arr(bisect) >= ins ))
            if (arr(bisect) >= ins) bisect = bisect - step;
            if (arr(bisect) < ins) bisect = bisect + step;
            if (step > 1) step = step/2;
        enddo
    else
        bisect = 1;
        do while (arr(bisect) < ins .and. bisect < arrL)
            bisect = bisect + 1;
        enddo
    endif
end function bisect

recursive subroutine quicksort(a, first, last)
     !Taken from: https://gist.github.com/t-nissie/479f0f16966925fa29ea
     !only used to sort the original (random) array later used to insert
     !elements into.
     implicit none
     real*8  a(*), x, t
     integer first, last
     integer i, j

     x = a( (first+last) / 2 )
     i = first
     j = last
     do
         do while (a(i) < x)
            i=i+1
         end do
         do while (x < a(j))
            j=j-1
         end do
         if (i >= j) exit
         t = a(i);  a(i) = a(j);  a(j) = t
         i=i+1
         j=j-1
     end do
     if (first < i-1) call quicksort(a, first, i-1)
     if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort
