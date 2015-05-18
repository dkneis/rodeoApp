module functions
  implicit none
  contains

  double precision function fun (conc,a,b,p,q)
    double precision, intent(in):: conc,a,b,p,q
    fun= 0.5d0 - (a + b * tanh(p * (log(conc + 1.1d0) - q)))
  end function

  double precision function fk (K,D,R,T)
    double precision, intent(in):: K,D,R,T
    fk= 1.d0 - (D + R + T) / K
  end function

end module

