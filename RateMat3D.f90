! Return the ratematix for a 3 dimentioan mof, The input of this subroutine is the postion of Ru and Os, 
! and the number of Ru and Os, the element energy transfer rates and  the total number of sites and the 
! length of the cubic of the 3D mof 

subroutine RateMat3D(n,num_Os_1d,transitionRate,SsiteRate,SingleRate,Os_idx,K)

   implicit none
   
   Integer :: n
   Integer :: a, b, c 
   Integer :: idx(7,3),idxs, idx_x1, idx_x2, idx_y1, idx_y2, idx_z1,idx_z2
   Integer :: site, neighor
   Integer :: num_Os_1d
   Integer :: Os_a_idx(num_Os_1d),Os_b_idx(num_Os_1d),Os_c_idx(num_Os_1d)
   Integer :: i, j, l, m ,o
   integer :: x_atom_type(n),y_atom_type(n),z_atom_type(n)
   Integer :: flag
   Integer :: neighor_Os_counter
   Real*8  :: transitionRate(2,2),SsiteRate(0:2,0:2), SingleRate(2) 
   Real*8  :: K(n*n*n,n*n*n)
   Integer :: Os_idx(num_Os_1d*num_Os_1d*num_Os_1d)
  
   ! Initialing the rate matrix
  idx(1,:)=[a,b,c]
  idx(2,:)=[a-1,b,c]
  idx(3,:)=[a+1,b,c]
  idx(4,:)=[a,b-1,c]
  idx(5,:)=[a,b+1,c]
  idx(6,:)=[a,b,c-1]
  idx(7,:)=[a,b,c+1]
  
  K=0
  Do a=1,n
     Do b=1,n
        Do c=1,n
                  idx(1,:)=[a,b,c]
                  idx(2,:)=[a-1,b,c]
                  idx(3,:)=[a+1,b,c]
                  idx(4,:)=[a,b-1,c]
                  idx(5,:)=[a,b+1,c]
                  idx(6,:)=[a,b,c-1]
                   idx(7,:)=[a,b,c+1]
                do i=1,7
                     do j=1,3
                        if (idx(i,j).gt.n) then
                           idx(i,j)=1
                        else if (idx(i,j).lt.1) then
                           idx(i,j)=n
                        end if 
                     end do
               end do 
           site=(idx(1,1)-1)*n*n+(idx(1,2)-1)*n+idx(1,3)
           do i=2,7
               neighor=(idx(i,1)-1)*n*n+(idx(i,2)-1)*n+idx(i,3)
          
               K(site,neighor)=transitionRate(1,1)
               K(neighor,site)=transitionRate(1,1)
           
           end do 
           
            K(site, site)=SingleRate(1)-4*transitionRate(1,1)
         
        end do  
     end do
  end do
  !*****************
  !*****************  
  call os_distri(n,num_Os_1d, Os_a_idx)
  call os_distri(n,num_Os_1d, Os_b_idx)
  call os_distri(n,num_Os_1d, Os_c_idx)
  !  x_atom_type is used to determine if the neighbor is Os.
  x_atom_type=0
  y_atom_type=0
  z_atom_type=0 
  do i=1,num_Os_1d 
  x_atom_type(Os_a_idx(i))=1
  y_atom_type(Os_b_idx(i))=1
  z_atom_type(Os_c_idx(i))=1
  end do
 
  do l=1,num_Os_1d
     do m=1,num_Os_1d
        do o=1,num_Os_1d
         a=Os_a_idx(l)
         b=Os_b_idx(m)
         c=Os_c_idx(o) 
         idx(1,:)=[a,b,c]
         idx(2,:)=[a-1,b,c]
         idx(3,:)=[a+1,b,c]
         idx(4,:)=[a,b-1,c]
         idx(5,:)=[a,b+1,c]
         idx(6,:)=[a,b,c-1]
         idx(7,:)=[a,b,c+1]
   
                  do i=2,7
                     do j=1,3
                        if (idx(i,j).gt.n) then
                           idx(i,j)=1
                        else if (idx(i,j).lt.1) then
                           idx(i,j)=n
                        end if
                     end do
                  end do
              

                site=(idx(1,1)-1)*n*n+(idx(1,2)-1)*n+idx(1,3)
                Os_idx((l-1)*num_Os_1d*num_Os_1d+(m-1)*num_Os_1d+o)=site
           neighor_Os_counter=0
           do i=2,7 
                flag=0
                neighor_Os_counter=0
                neighor=(idx(i,1)-1)*n*n+(idx(i,2)-1)*n+idx(i,3)
                flag=x_atom_type(idx(i,1))*y_atom_type(idx(i,2))*z_atom_type(idx(i,3)) ! determine nearest neigbor atom type
                neighor_Os_counter=neighor_Os_counter+1
                if (flag.eq.0) then
                
                K(site,neighor)= transitionRate(1,2)
                K(neighor,site)= transitionRate(2,1)
               else 
                K(site,neighor)= transitionRate(2,2)
                K(neighor,site)= transitionRate(2,2)
                end if 
          end do 
            K(site,site)=-(4-neighor_Os_counter)*transitionRate(2,1)-neighor_Os_counter*transitionRate(2,2)+SsiteRate(1,1)
end do 
end do 
end do 
                    
end subroutine RateMat3D         
  
