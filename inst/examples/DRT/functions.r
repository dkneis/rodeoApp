fun= function(A,a,b,p,q) {
  0.5 - (a + b * tanh(p * (log(A + 1.1) - q)))
}

fk= function(K,D,R,T) {
  1 - (D + R + T) / K
}

