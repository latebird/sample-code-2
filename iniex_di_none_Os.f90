! Return the index of the Os.
subroutine iniex_di_none_Os(lengthmof,num_Os,num_excite,m,inexidx)

implicit none

   integer :: lengthmof, num_excite,num_Os,temp
   integer :: i,j,k
   integer :: flag1, flag2
   integer :: idx
   integer :: m(num_Os), m_temp1(num_Os),m_temp(num_Os), inexidx(num_excite)
   real*8  :: temp_real
   
    
      m_temp1=m
   

   do i=num_Os,1,-1
      do j=1,i-1
        if (m_temp1(j).gt.m_temp1(j+1)) then
         m_temp1(j)=m_temp1(j)+m_temp1(j+1)
         m_temp1(j+1)=m_temp1(j)-m_temp1(j+1)
         m_temp1(j)=m_temp1(j)-m_temp1(j+1)    
        end if   
      end do 
   end do

!print *, m_temp1

m_temp=m_temp1
!stop
    flag1 = 0
 
    Do while (flag1.eq.0)

         Call random_number(temp_real)
         temp=int(temp_real*dble(lengthmof-num_Os-1))+1

         Do j=1,num_Os

            If (temp.lt.m_temp(1)) then
               idx=temp
               flag1=1
               exit
            else if (temp.gt.m_temp(num_Os)) then
               idx=temp+num_Os
               flag1=1
               exit
            else if ((m_temp(j).lt.(temp+j)).and.(m_temp(j+1).gt.(temp+j))) then
               idx=temp+j
               flag1=1
               exit
            end if

         end do
            
         inexidx(1)=idx

     end do

!%%%%%%%%%%%%%%%%%%%%%%%%%

Do i=2, num_excite
 flag1=0
      Do while ((flag1.eq.0).or.(flag2.eq.0))
          
         flag1=0  
         flag2=1
         
         Call random_number(temp_real)
         temp=int(temp_real*dble(lengthmof-num_Os-1))+1  
         idx=0 
         Do j=1,num_Os
            
            If (temp.lt.m_temp(1)) then 
               idx=temp
               flag1=1
               exit
            else if (temp.gt.m_temp(num_Os)) then
               idx=temp+num_Os
               flag1=1
               exit
            else if ((m_temp(j).lt.(temp+j)).and.(m_temp(j+1).gt.(temp+j))) then
               idx=temp+j
               flag1=1
               exit
            end if
                 
         end do
         
         
         If (flag1.eq.1) then

             Do k=1,i-1
                
                if (idx.eq.inexidx(k)) then 
                    flag2=0
                end if 
        
             end do
       
         end if
        
        If ((flag1.eq.1).and.(flag2.eq.1)) then
              inexidx(i)=idx
        end if 
      end do                

end do
!%%%%%%%%%%%%%%%%%%%%%
end subroutine iniex_di_none_Os


