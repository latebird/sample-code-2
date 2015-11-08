PROGRAM M_One_Main

  implicit none

  integer, parameter :: atomtype = 2  ! 1: Ru; 2: Os

  integer         :: lengthmof,    NumChain,     NumIniEx,  tsteps
  integer         :: site, num_Os,  num_excite
  integer         :: i, j,  kk,  n, temp, ll,  h, ii
  integer  :: u1,v1
  integer, allocatable :: m(:) ! Os site index
  
  real*8, allocatable :: K(:,:),  T(:), U(:,:), Fi(:), K0(:,:)
  real*8, allocatable :: work(:, :), Vinv(:, :)
  real*8, allocatable :: V(:,:),  D(:,:)

  real*8              :: kRuhop,  kRuDecay, kOshop, kOsDecay,  kOsTrap,   kOsUntrap  
  real*8              :: Ospctg,  inExPctg 
  real*8              :: Tmax, Fimax

  real*8   :: transitionRate(atomtype, atomtype)
  real*8   :: SingleRate(2),  SsiteRate(0:atomtype,0:atomtype)

  character(len=30)   :: ouputfilename
  
  namelist /input/kRuhop, kRuDecay, kOshop, kOsDecay, kOsTrap, kOsUntrap, lengthmof, &
                  Ospctg,  tsteps, inExPctg, NumChain, NumIniEx, ouputfilename
  kRuhop        = 1./10.
  kRuDecay      = 1./375.
  kOshop        = 1./10.
  kOsDecay      = 1./11.
  kOsTrap       = 1./30.
  kOsUntrap     = 0.00001
  lengthmof     = 1000
  Ospctg        = 0.05
  tsteps        = 200
  inExPctg      = 0.5
  NumChain      = 10
  NumIniEx      = 10
  ouputfilename = '3D_test_1.dat'

  read (*, input)

  n = lengthmof
  allocate(K(n,n))
  allocate(Fi(tsteps))
  allocate(T(tsteps))
  allocate(U(n,n))
  allocate(work(n,n))
  allocate(V(n,n))
  allocate(D(n,n))
  allocate(Vinv(n,n))
  allocate(K0(n,n))
  

  num_Os = int(dble(lengthmof) * Ospctg)+1
  allocate(m(num_os))
  !call checkifOspresent(num_Os)
  num_excite = int(dble(lengthmof) * Inexpctg)
  !call checkifexcited(num_excite)
  call rateconstants(kOsTrap, kOsUntrap, kOsDecay, kOshop, kRuhop, kRuDecay, &
                     SingleRate,transitionRate, SsiteRate) 
  call basicKmatrix(lengthmof, SingleRate, K0)
  Fi(1:tsteps) = 0.0d+00
  DO  i=  1, NumChain
      ! K = K0
       !call RateMatrixauto(lengthmof, transitionRate, SsiteRate, num_Os, &
        !                   m, K)  
       call RateMat3D(8,2,transitionRate,SsiteRate,SingleRate,m,K)
       !open(22, file="3DRateMat", status='unknown')
       !do u1=1,n
        !   do v1=1,n
         !   write (22, '(I6,I6, E20.10)') u1,v1, K(u1,v1)
          ! end do
     !  end do
      ! close(22)
       call my_diag(lengthmof, K, V, D)
       call my_matrix_inv(lengthmof, V, Vinv)
  
       DO j = 1, lengthmof 
              D(j,j) = exp(D(j,j))
       END Do
       
       work = matmul(V,D)
       U = matmul(work,Vinv)
       Call iniexloop(NumIniEx, lengthmof, tsteps, num_Os, num_excite,  U, m, T)
     Fi = Fi + T
  END DO

  call findmax(tsteps, Fi, Fimax)
  Fi = Fi / Fimax 

  open(22, file=ouputfilename, status='unknown')
       do i = 1, tsteps
             write (22, '(I6, E20.10)') i, Fi(i)
       end do
  close(22)

END PROGRAM
