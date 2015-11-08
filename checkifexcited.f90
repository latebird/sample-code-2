subroutine checkifexcited(a)
implicit none

integer :: a

 if (a .lt. 1) then
     print *, " No excitation"
     stop
  end if

end subroutine checkifexcited
               
