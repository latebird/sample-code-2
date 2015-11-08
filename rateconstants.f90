subroutine rateconstants(kOsTrap, kOsUntrap, kOsDecay, kOshop, kRuhop, kRuDecay, &
                         SingleRate,transitionRate, SsiteRate)
 implicit none

 integer, parameter :: atomtype = 2  ! 1: Ru; 2: Os

 real*8   :: transitionRate(atomtype, atomtype)
 real*8   :: SingleRate(2), SsiteRate(0:atomtype,0:atomtype)

 real*8   :: kOsTrap, kOsUntrap, kOsDecay, kOshop, kRuhop, kRuDecay

 transitionRate = 0.0

 transitionRate(1,1) = kRuhop
 transitionRate(1,2) = kOsTrap
 transitionRate(2,1) = kOsUntrap
 transitionRate(2,2) = kOshop

 SingleRate(1) = -2.0*kRuhop-kRuDecay  ! diagonal rate
 SingleRate(2) = kRuhop

 SsiteRate(1, 1) = -2.0*kOsUntrap-kOsDecay
 SsiteRate(1, 2) = -kOsUntrap-kOshop-kOsDecay
 SsiteRate(2, 1) = -kOsUntrap-kOshop-kOsDecay
 SsiteRate(2, 2) = -2.0*kOshop-kOsDecay
 SsiteRate(0, 1) = -kOsUntrap-kOsDecay
 SsiteRate(0, 2) = -kOshop-kOsDecay
 SsiteRate(1, 0) = -kOsUntrap-kOsDecay
 SsiteRate(2, 0) = -kOshop-kOsDecay

end subroutine rateconstants



