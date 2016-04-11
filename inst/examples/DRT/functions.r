rspMIC = function (c, mic) {
  return(max(0, 1 - c/mic))
}

rspSig = function (c, c50, slp) {
  sig= 1 / (1 + exp(slp * (c - c50)))       # Original curve
  sig0= 1 / (1 + exp(slp * (0 - c50)))
  return(sig / sig0)
}

rsp = function (c, mic, use_mic, c50, slp) {
  if (use_mic > 0) {
    return(rspMIC(c, mic))
  } else {
    return(rspSig(c, c50, slp))
  }
}

fk= function(K,D,R,T) {
  return(1 - (D + R + T) / K)
}

