Subroutine RateContMartix(lengthmof, kRuhop, kRuDecay,kOshop,kOsDecay,kOsTrap, &
                          kOsUntrap,Atp,num_Os, m, K)

  IMPLICIT NONE
  integer  :: num_Os
  integer  :: lengthmof, i, j, n
  integer  :: m(lengthmof)                      ! Os site index
  integer  :: site
  real*8   :: K(lengthmof,lengthmof)
  real*8   :: Atp(lengthmof)
  real*8   :: kRuhop, kRuDecay,kOshop,kOsDecay,kOsTrap, kOsUntrap
  real*8   :: tmp_1, tmp_2, tmp_3, tmp_4,left, right
  real*8   :: tempa(num_Os)
  n     = lengthmof
  Atp   = 0.0
  m     = 0
  K     = 0.0
  tmp_1 = -2.0*kRuhop-kRuDecay
  tmp_2 = -2.0*kOsUntrap-kOsDecay
  tmp_3 = -kOsUntrap-kOshop-kOsDecay
  tmp_4 = -2.0*kOshop-kOsDecay
  Call random_number(tempa)
!Creat Atom type chain index array 
  Do i=1, num_Os
    j =int(dble(lengthmof-1)*tempa(i))+1
    Atp(j)=1.0
    m(i)=j
  End Do

! Dignoal elements
  do i = 1, lengthmof-1
     K(i,i)   =  tmp_1
     K(i,i+1) =  kRuhop
     K(i+1,i) =  kRuhop
  end do
  K(lengthmof,lengthmof)=tmp_1
 
!Non-Diagnoal elements
  do i = 1, num_Os
     site = m(i)

     ! if =0:Ru; =1:Os 
     call checksite(lengthmof, num_Os, Atp, site-1, left) 
     call checksite(lengthmof, num_Os, Atp, site+1, Right)

     if ((left.eq.0).and.(Right.eq.0)) then          ! ! Ru-Os-Ru
        K(site,site)  =tmp_2
        K(site,site-1)=kOsTrap
        K(site,site+1)=kOsTrap
        K(site-1,site)=kOsUntrap
        K(site+1,Site)=kOsUntrap
     else if ((left.eq.0).and.(Right.eq.1)) then     ! Ru-Os-Os
        K(site,site)=tmp_3
        K(site,site-1)=kOsTrap
        K(site,site+1)=kOshop
        K(site-1,site)=kOsUntrap
        K(site+1,Site)=kOshop        
     else if ((left.eq.1).and.(Right.eq.0)) then     ! Os-Os-Ru
        K(site,site)=tmp_3
        K(site,site+1)=kOsTrap
        K(site,site-1)=kOshop
        K(site+1,site)=kOsUntrap
        K(site-1,Site)=kOshop
     else if ((left.eq.1).and.(Right.eq.1)) then     ! Os-Os-Os
        K(site,site)=tmp_4
        K(site,site-1)=kOshop
        K(site,site+1)=kOshop
        K(site-1,site)=kOshop
        K(site+1,Site)=kOshop
! Beginning of the chain and the end of the chain
     else if ((left.eq.-1).and.(Right.eq.1)) then
        K(site,site)=-kosdecay-koshop
        K(site,site+1)=kOshop
        K(site+1,site)=KOshop
     else if ((left.eq.-1).and.(Right.eq.0)) then
        K(site,site)=-kosdecay-koshop
        K(site,site+1)=kOstrap
        K(site+1,site)=KOsuntrap
     else if ((right.eq.-1).and.(left.eq.0)) then
        K(site,site)=-kosdecay-koshop
        K(site,site-1)=kOstrap
        K(site-1,site)=KOsuntrap
     else if ((right.eq.-1).and.(left.eq.1)) then
        K(site,site)=-kosdecay-koshop
        K(site,site-1)=kOshop
        K(site-1,site)=KOshop
     end if

  end do

    !if (xyz.eq.25) stop

!!$  if (debug) then
!!$
!!$   open(102, file="Os_index", status='unknown')
!!$
!!$   Do i=1, lengthmof
!!$     write (102, '(i6, i6)') i, m(i)
!!$   end do
!!$
!!$   close(102)
!!$
!!$   open(109, file="Atp_index", status='unknown')
!!$
!!$   Do i=1, lengthmof
!!$     write (109,  '(i6, E20.10)')i, Atp(i)
!!$   end do
!!$
!!$   close(109)
!!$
!!$ end if



end subroutine RateContMartix


subroutine checksite(lengthmof, num_os, Atp, isite, iflag)

 implicit none

 integer :: num_os, isite, iflag, lengthmof
 integer :: Atp(lengthmof)

 integer :: i
 
 iflag = 1   !  iflag = 0 : Ru; iflag = 1; Os
 
 if (isite .eq. 0) then
    iflag = -1
    return
 end if

 if (isite .eq. (lengthmof+1)) then
    iflag = -1
    return
 end if

if (Atp(isite).gt.0.0) then
   iflag=2
  return
end if


end subroutine checksite
