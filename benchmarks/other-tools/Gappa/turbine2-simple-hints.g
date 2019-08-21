@rnd = float<ieee_64,ne>;

v = rnd(Mv);
w = rnd(Mw);
r = rnd(Mr);

r2 rnd= 6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5;
Mr2 = 6*Mv - 0.5 * Mv * (Mw*Mw*Mr*Mr) / (1-Mv) - 2.5;

{ Mv in [-4.5, -0.3] /\
  Mw in [0.4, 0.9] /\
  Mr in [3.8, 7.8] ->
    |r2 - Mr2| in ? }

$ Mv;
$ Mw;
$ Mr;
