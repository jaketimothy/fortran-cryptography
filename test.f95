program datastructures
    use, intrinsic :: iso_fortran_env
    use primes
    use sha1Module
    use rsaModule
    implicit none
    
    print *, "isPrime(67)"
    print *, isPrime(67_int8)
    
    print *, "primesUpTo(255)"
    print *, primesUpTo(255_int32)
    
    print *, "message"
    print *, SHA1("message")
    
    print *, ""
    print *, SHA1("")
    
    print *, 0_int64
    print *, SHA1(0_int64)
    
    print *, 700_int64
    print *, SHA1(700_int64)
    
    print *, rsaGenerateKeys()
 end program datastructures
