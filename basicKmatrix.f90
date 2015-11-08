  subroutine basicKmatrix(lengthmof, SingleRate, K)

   implicit none

   integer :: lengthmof

   real*8  :: SingleRate(2)
   real*8  :: K(lengthmof, lengthmof)

   integer :: i

   K = 0.0d+00

   do i = 1, lengthmof - 1
      K(i,i)   = SingleRate(1)
      k(i,i+1) = SingleRate(2)
      k(i+1,i) = k(i,i+1)
   end do

   K(lengthmof, lengthmof) = SingleRate(1)

 end subroutine basicKmatrix 
