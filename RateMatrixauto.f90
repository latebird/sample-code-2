subroutine RateMatrixauto(lengthmof,transitionRate, SsiteRate, num_Os, m, K)

  IMPLICIT NONE

  integer, parameter :: atomtype = 2  ! 1: Ru; 2: Os

  real*8   :: transitionRate(atomtype,atomtype)
  real*8   :: SsiteRate(0:atomtype,0:atomtype)

  integer  :: num_Os
  integer  :: lengthmof, i, j, n, left, right
  integer  :: m(num_Os)                      ! Os site index
  integer  :: site
  
  integer  :: u,v
  real*8   :: K(lengthmof,lengthmof)
  real*8   :: tempa(num_Os)

  n     = lengthmof
  m     = 0
  
  ! Call random_number(tempa)
   !      Do i=1, num_Os
    !          m(i)=int(dble(lengthmof-1)*tempa(i))+1
     !    End Do
   Call Os_distri(lengthmof, num_Os, m)
  
   do i = 1, num_Os
       site = m(i)
       ! if =1:Ru; =2:Os 
       call checksite(lengthmof, num_Os, m, site-1, left)  
       call checksite(lengthmof, num_Os, m, site+1, Right)

       if (left.ne.0)   then
            K(site,site-1) = transitionRate(left,  2)
            K(site-1,site) = transitionRate(2, left)
       end if

       if (Right.ne. 0) then
             K(site,site+1) = transitionRate(right, 2)
             K(site+1,Site) = transitionRate(2, right) 
       end if 
  
       K(site,site)   = SsiteRate(left, right)
        open(22, file="rate_matrix", status='unknown')
       do u = 1, n
          do v= 1, n
             write (22, '(I6,I6, E20.10)') u,v, K(u,v)
         end do
       end do
        close(22)
       
 
   end do

end subroutine RateMatrixauto
