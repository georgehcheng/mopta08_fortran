
program mopta08

    implicit      none

    character*200  in_file
    character*200  out_file
    character(len=1000) :: line
    character(len=20) :: label
    character(len=1) :: delimiter
    integer        nvar
    integer        ncon
    parameter      (nvar = 124)
    parameter      (ncon =  68)
    integer        i
    real*8         x(nvar)
    real*8         f
    real*8         g(ncon)
    
    in_file  = "input.txt"
    out_file = "output.txt"
    
    delimiter = "="

    open(7,FILE=in_file)
    read(7,'(A)') line
    close(7)
    do i=1,len_trim(line)
        if(line(i:i) == delimiter)then
            line(i:i) = " "
        endif
    enddo
    do i=1,nvar
        read(line,*) label,x(i)
    enddo

    call func(nvar,ncon,x,f,g)

    open(8,FILE=out_file)
    write(8,*) "f =",f,char(13)//char(10)
    do i=1,ncon
        write(label,'(i2)') i
        write(8,*) "c"//adjustl(label)//"=",g(i),char(13)//char(10)
    enddo
    close(8)

    stop 0

end
