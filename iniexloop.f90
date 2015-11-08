Subroutine iniexloop(NumIniEx, lengthmof, tsteps, num_Os, num_excite, U, m, T)

     implicit none

     Integer  :: NumIniex, tsteps, lengthmof,num_Os, num_excite
     Integer  :: m(num_Os)
     integer  :: inexidx(num_excite)
     Real*8   :: U(lengthmof, lengthmof)
    
     Integer  :: k,h,i,temp
     Real*8   :: P(lengthmof, tsteps)
     Real*8   :: C0(lengthmof)
     Real*8   :: F(tsteps)
     Real*8   :: T(tsteps)
     Real*8   :: Tmax, Fmax
!print *, NumIniEx, lengthmof, tsteps, num_Os, num_excite
!       print *, shape(U),shape(m),shape(T)
T(1:tsteps) = 0.0

Do h=1,NumIniEx
   C0=0.01
   F(1:tsteps) = 0.0
   !Call initial_di(lengthmof,num_excite, C0)
   Call iniex_di_none_Os(lengthmof,num_Os,num_excite,m,inexidx)
   

 !  do i =1,num_excite
!   print *, inexidx(i) 
  ! end do  
   

   do i=1, num_excite
      temp=inexidx(i)
      C0(temp)=10.0
   end do

   P(1:lengthmof,1)=C0
   Do k=2,tsteps 
          P(:,k)=matmul(U,P(:,k-1))
   End Do

   Do k=1,num_Os
      F=P(m(k), 1:tsteps)+F
   End Do
   call findmax(tsteps,F,Fmax)
   F=F/Fmax
   T = T+F 
End Do

Call findmax(tsteps, T, Tmax)
   T=T/Tmax
!stop
End Subroutine iniexloop 
