program datastructures
    use, intrinsic :: iso_fortran_env
    use crypto
    implicit none
    
    print *, "message"
    print *, SHA1("message")
    
    print *, ""
    print *, SHA1("")
    
    print *, 0_int64
    print *, SHA1(0_int64)
    
    print *, 700_int64
    print *, SHA1(700_int64)
 end program datastructures
