program datastructures
    use, intrinsic :: iso_fortran_env
    use crypto
    implicit none
    
    character(len=*), parameter :: val1 = "message"
    character(len=*), parameter :: val2 = ""
    character(len=8) :: inval = "FFFF8001"
    integer(int32) :: itest
    
    read(inval, "(Z8)") itest
    print *, inval
    print *, itest
    print "(Z8.8)", itest
    
    print *, "----"
    print *, val1
    print *, SHA1(val1)
    
    print *, "----"
    print *, val2
    print *, SHA1(val2)
end program datastructures