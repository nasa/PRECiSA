@rnd = float<ieee_64,ne>;

r = rnd(Mr);
K = rnd(MK);
x = rnd(Mx);

res rnd= (r*x) / (1 + (x/K));
Mres = (Mr*Mx) / (1 + (Mx/MK));

{ Mr in [4.0, 4.0] /\
  MK in [1.11, 1.11] /\
  Mx in [0.1, 0.3] 
    -> |res - Mres| in ? }

$ Mx;
