! Notes:
! No, fortran does not support unsigned integers.

module crypto
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    
    public :: SHA1
    
contains
    function SHA1(string)
        character(len=*), intent(in) :: string
        character(len=40) :: SHA1
        
        integer(int32) :: h0, h1, h2, h3, h4, a, b, c, d, e, f, k, temp
        integer(int32), dimension(80) :: w
        integer(int64) :: length
        integer(int8), dimension(:), allocatable :: ipadded
        integer :: i, j
        
        length = len_trim(string) ! TODO : support string lengths possible with uint64
        
        allocate(ipadded(((length+8)/64 + 1)*64))
        ipadded(:length) = transfer(string, ipadded(:length))
        ipadded(length+1) = ibset(0_int8, 7)
        ipadded(length+2:) = 0_int8
        ipadded(size(ipadded)-7:) = transfer(length*8_int64, ipadded(size(ipadded)-7:))
        ipadded(size(ipadded)-7:) = ipadded(size(ipadded):size(ipadded)-7:-1)
        
        h0 = 1732584193_int32 ! z'67452301'
        h1 = -271733879_int32 ! z'EFCDAB89'
        h2 = -1732584194_int32 ! z'98BADCFE'
        h3 = 271733878_int32 ! z'10325476'
        h4 = -1009589776_int32 ! z'C3D2E1F0'
        
        do i = 1,size(ipadded)/64
            do j = 1,16 ! take 512 bit chunk of string
                w(j) = transfer(ipadded((i-1)*64 + j*4:(i-1)*64 + (j-1)*4 + 1:-1), w(j)) ! is the source size less than the result size?
            end do
            do j = 17,80 ! Extend the sixteen 32-bit words into eighty 32-bit words
                w(j) = ishftc(ieor(ieor(ieor(w(j-3), w(j-8)), w(j-14)), w(j-16)), 1)
            end do
            
            a = h0; b = h1; c = h2; d = h3; e = h4
            do j = 1,80
                select case (j)
                    case (1:20)
                        f = ior(iand(b, c), iand(not(b), d))
                        k = 1518500249_int32 ! k = z'5A827999'
                    case (21:40)
                        f = ieor(ieor(b, c), d)
                        k = 1859775393_int32 ! k = z'6ED9EBA1'
                    case (41:60)
                        f = ior(ior(iand(b, c), iand(b, d)), iand(c, d))
                        k = -1894007588_int32 ! k = z'8F1BBCDC'
                    case (61:80)
                        f = ieor(ieor(b, c), d)
                        k = -899497514_int32 ! k = z'CA62C1D6'
                end select
                
                temp = ishftc(a, 5) + f + e + w(j) + k
                e = d
                d = c
                c = ishftc(b, 30)
                b = a
                a = temp
            end do
            
            h0 = h0 + a
            h1 = h1 + b
            h2 = h2 + c
            h3 = h3 + d
            h4 = h4 + e
        end do
        
        deallocate(ipadded)
        
        write(SHA1(1:8), "(Z8.8)") h0
        write(SHA1(9:16), "(Z8.8)") h1
        write(SHA1(17:24), "(Z8.8)") h2
        write(SHA1(25:32), "(Z8.8)") h3
        write(SHA1(33:40), "(Z8.8)") h4
    end function SHA1
end module crypto
