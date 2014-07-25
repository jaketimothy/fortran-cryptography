program datastructures
    use hashtable_mod
    implicit none
    
    integer :: i
    type(hashtable), target :: mytable
    character(len=32), target :: key, outval
    integer :: value
    
    do i = 1,12
        write(key, '(i2)') i
        call mytable%put(key, i)
    end do
    call mytable%get(key, value)
    print *, 'value', value
    do i = 13,32
        write(key, '(i2)') i
        call mytable%put(key, i)
    end do
    call mytable%put('me', 'jake')
    call mytable%put('jakesemail', 'xxx@xxx.com')
    
    call mytable%get(key, value)
    print *, 'value', value
    
    call mytable%get('jakesemail', outval)
    print *, 'value', outval
    
    call mytable%dispose()
        
end program datastructures