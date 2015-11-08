!
! utility routines
!
! =====================================
!
subroutine my_print(ichannel, n, a)

 implicit none

 integer :: ichannel, n
 integer :: j, i
 real*8  :: a(n,n)

 do i = 1, n
     do j = 1, n
         write (ichannel, '(2i6,E30.10)') i,j,a(i,j)
     end do
end do

end subroutine my_print
!
! =====================================
!
subroutine findmax(n, a, amax)

 implicit none

 integer :: n
 real*8  :: a(n)
 real*8  :: amax

 integer :: i
 real*8  :: tmp_1

 tmp_1 = a(1)

 do i = 2, n
    if (a(i) .gt. tmp_1) then
       tmp_1 = a(i)
    end if
 end do

 amax = tmp_1

end subroutine findmax
!
! =====================================
!
! inverse of a general real matrix
!
subroutine my_matrix_inv(n, a, ainv)

 implicit none

 integer :: n
 real*8  :: a(n,n), ainv(n,n)
  
 integer :: lda, ipiv(n), info, LWORK
 real*8  :: work(n)

 lda = n
 ainv = a

 call DGETRF(n, n, ainv, LDA, IPIV, INFO )

 if (INFO .ne. 0) then
   print *, "error in LU decomp, error code = ", INFO
   stop
 end if

 LWORK = n
 call dgetri( N, ainv, LDA, IPIV, WORK, LWORK, INFO )
 
 if (INFO .ne. 0) then
   print *, "error inverse, error code = ", INFO
   stop
 end if

end subroutine my_matrix_inv
!
!-------------------------------------------------------
!
subroutine my_diag(n, a, vecs, eigenvalue)

 implicit none

 integer :: n
 real*8  :: a(n,n), vecs(n,n), eigenvalue(n, n)

 character (len=1) :: JOBVL, JOBVR
 integer :: LDA, LDB, LDVR, LDVL, LWORK, INFO

 real*8 :: aa(n,n), bb(n, n), ALPHAR(n), ALPHAI(n), BETA(n)
 real*8 :: VR(n, n), VL(n, n)
 real*8, allocatable :: work(:)
 real*8, parameter :: thresh = 1.d-10

 integer ::i,j

 JOBVL = 'V'
 JOBVR = 'V'
 LDA   = n
 LDB   = n
 LDVL  = n
 LDVR  = n
 LWORK = 8 * n + 1
 
 bb         = 0.0
 do i = 1, n
    bb(i,i) = 1.0d+00
 end do

 eigenvalue = 0.0

 allocate(work(LWORK))

 aa = a

      !open(101, file="RateContMatrix.ERR", status='OLD')
      !do i=1,n
         !Do j=1,n
          !read (101, '(E20.10)')  aa(i,j)
         !end do
         !read (101, *)
     !end do
     !close(101)


 call DGGEV(JOBVL, JOBVR, N, AA, LDA, BB, LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )

 if (INFO .ne. 0) then
   print *, "error in Matrix Diag, error code = ", INFO
   stop
 end if

 do LDA = 1, n

    if (abs(ALPHAI(LDA)) .gt. thresh) then

      print *, LDA, ALPHAI(LDA)
  
      print *, "Complex EigenValue"
      open(101, file="RateContMatrix", status='unknown')
      do i=1,n
         Do j=1,n
          write (101, '(E20.10)')  a(i,j)
         end do
         Write (101, *)
      end do
      close(101)

      stop

   end if

   eigenvalue(LDA, LDA) = ALPHAR(LDA)

 end do

 vecs = VR

 deallocate(work)

end subroutine my_diag
!
!-------------------------------------------------------
!
subroutine my_matrix_multix(row1, col1, row2, col2, a, b, c)

 implicit none
 
 integer :: row1, col1, row2, col2
 real*8  :: a(row1, col1), b(row2, col2), c(row1, col2)


 integer :: m, n, k, lda, ldb, ldc
 real*8  :: alpha, beta
 character (len=1) :: TRANSA, TRANSB


 if (col1 .ne. row2) then
    print *, " col1 must equal to row2"
    stop
 end if

 TRANSA='n'
 TRANSB='n'
 
 m = row1
 n = col2
 k = col1

 alpha = 1.0d+00
 beta  = 0.0d+00

 lda   = m
 ldb   = k
 ldc   = m

 call DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC) 

end subroutine my_matrix_multix 
