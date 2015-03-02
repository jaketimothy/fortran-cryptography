program datastructures
    use crypto
    implicit none
    
    print *, "----"
    print *, "message"
    print *, SHA1("message")
    
    print *, "----"
    print *, ""
    print *, SHA1("")
    
    print *, "----"
    print *, "The quick brown fox jumps over the lazy dog"
    print *, SHA1("The quick brown fox jumps over the lazy dog")
end program datastructures
