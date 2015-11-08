SUBROUTINE initial_di(n, num_ex, C0)

IMPLICIT NONE
INTEGER               :: n,ii, num_ex
real*8                :: C0(n)
Integer               :: tmp_1
real*8                :: temp(num_ex)
!integer               :: iflag
 C0=0.1
 Call random_number(temp)
 temp = temp * dble(n-1)

 Do ii=1,num_ex

    tmp_1 =int(temp(ii))+1

   ! call checkOs(tmp_1,num_os, m, iflag)

    !if (iflag .eq. 1) then
     !  C0(tmp_1)=0.1
   ! else if (iflag .eq. -1) then
       C0(tmp_1)=10.
   ! end if

 End Do
END Subroutine initial_di


subroutine checkOs(ix, num_os, m, iflag)

 implicit none

 integer ::  ix, num_os
 integer ::  m(num_os)
 integer ::  iflag, i

 iflag = -1

 do i = 1, num_os
    if (ix .eq. m(i)) then
       iflag = 1
       return
    end if
 end do

end subroutine checkOs
