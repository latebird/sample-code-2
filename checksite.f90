! helper subroutine to check it whether is a Ru or Os near by a target site.
subroutine checksite(n, num_Os, m, site, iflag)

 implicit none

 integer :: n ,i,num_Os
 integer :: site, iflag
 integer :: m(num_Os)


 if ((site .eq. 0).or.(site .eq. n+1))   then
    iflag = 0
    return
 end if
 
 iflag = 1   !  iflag = 1 : Ru; iflag = 2; Os
 
 do i = 1, num_Os
    if (site.eq.m(i)) then
       iflag = 2
       return
    end if
 end do
 
end subroutine checksite
