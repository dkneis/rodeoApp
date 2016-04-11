module functions
  implicit none
  contains

  ! Response curve based on MIC, e.g. Hellweger 2011
  function rspMIC (c, mic) result (res)
    double precision, intent(in):: c, mic
    double precision:: res
    res=   max(0.d0, 1.d0 - c/mic)
  end function

  ! Sigmoidal response curve analogous to Eqn. 4 in Liu et al., 2004,
  ! DOI: 10.1128/AAC.48.10.3884-3891.2004
  ! Rescaled to precisely return 1 at c=0
  function rspSig (c, c50, slp) result (res)
    double precision, intent(in):: c, c50, slp
    double precision:: res, sig, sig0
    sig= 1.d0 / (1.d0 + exp(slp * (c - c50)))       ! Original curve
    sig0= 1.d0 / (1.d0 + exp(slp * (0.d0 - c50)))
    res= sig / sig0
  end function

  ! Generic functional response
  function rsp (c, mic, use_mic, c50, slp) result (res)
    double precision, intent(in):: c, mic, use_mic, c50, slp
    double precision:: res
    if (use_mic .gt. 0.d0) then
      res= rspMIC(c, mic)
    else
      res= rspSig(c, c50, slp)
    end if
  end function

  function fk (K,D,R,T) result (res)
    double precision, intent(in):: K,D,R,T
    double precision:: res
    res= 1.d0 - (D + R + T) / K
  end function

end module

