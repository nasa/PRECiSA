@rnd = float<ieee_64,ne>;

Mk = 1.3806503e-23;
k = rnd(Mk);

T = rnd(MT);
a = rnd(Ma);
b = rnd(Mb);
N = rnd(MN);
p = rnd(Mp);
V = rnd(MV);

res rnd= (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T;
Mres = (Mp + Ma * (MN / MV) * (MN / MV)) * (MV - MN * Mb) - Mk * MN * MT;

{ MT in [300.0, 300.0] /\
  Ma in [0.401, 0.401] /\
  Mb in [42.7e-6, 42.7e-6] /\
  MN in [1000, 1000] /\
  Mp in [3.5e7, 3.5e7] /\
  MV in [0.1, 0.5] 
  -> |res - Mres| in ?}

$ MV;

