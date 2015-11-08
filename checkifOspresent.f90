subroutine checkifOspresent(a)

implicit none

integer :: a

 if (a .lt. 1) then
     print *, " No Os is present"
     stop
  end if
end subroutine checkifOspresent 
