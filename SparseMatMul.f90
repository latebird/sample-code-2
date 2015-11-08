subroutine SparseMatMul( Num_Row, Num_None_Zero_Column,MatNoneZeroValue, MatNoneZeroValue_idx,Vector_in,Verctor_out)
  !How to map the idx of the none zero element in the rate matrix  F(i,:)=[none zero cloumn]

  implicit none
  
  Integer :: Num_Row,Num_None_Zero_Column(Num_Row)
  Integer :: MatNoneZeroValue_idx
  Real*8  :: Vector_in(Num_Row), Vector_out(Num_Row) 
  Real*8  :: MatNoneZeroValue(Num_Row,Num_None_Zero_Column)
  
  Integer :: i, j
  
  Real*8  :: temp
  do i=1,Num_Row
  temp=0.0d+00
  do j=1,Num_None_Zero_Coulmn(j)
  temp =temp+ MatNoneZeroValue(i,j)*Vector_in(MatNoneZeroValue_idx(i,j))
  
  end do 
  Vector_out(i)=temp
  end do
  

end subroutine SparseMatmul 
   
