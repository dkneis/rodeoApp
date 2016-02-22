module functions
  implicit none
  contains

  double precision function fa (A,mic)
    double precision, intent(in):: A, mic
    fa=   max(0.d0, 1.d0 - A/mic)
  end function

  double precision function fk (K,D,R,T)
    double precision, intent(in):: K,D,R,T
    fk= 1.d0 - (D + R + T) / K
  end function

end module

