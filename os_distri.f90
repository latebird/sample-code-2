
! Return the Os site index in the MOF
subroutine Os_distri(lengthmof, num_Os, m)


implicit none

    integer   :: lengthmof, num_Os
    integer   :: m(num_Os)
    integer   :: i, j
    integer   :: temp_int
    integer   :: flag 
    real*8    :: temp
   

    m=0
        call random_number(temp)
        m(1)=int(temp*dble(lengthmof-1))+1    

Do i=1, num_Os
    
    flag = 0
    
    Do while (flag.eq.0)   
            
        flag=1

        call random_number(temp)
        temp_int=int(temp*dble(lengthmof-1))+1
        

        Do j=1,i

           if (temp_int.eq.m(j))then 
               flag=0 
               exit
           end if

        end do 
        
 
        if (flag.eq.1) then 
           m(i+1)=temp_int
        end if 
   end do


end do


end subroutine Os_distri
