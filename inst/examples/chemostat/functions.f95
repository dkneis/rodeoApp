module functions
implicit none

contains

double precision function monod(s, h)
double precision, intent(in):: s, h
monod = s / (s + h)
end function

end module
