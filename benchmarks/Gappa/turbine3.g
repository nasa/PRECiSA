@rnd = float<ieee_64,ne>;

v = rnd(Mv);
w = rnd(Mw);
r = rnd(Mr);

r3 rnd= 3 - 2 / (r * r) - 0.125 * (1 + 2 * v) * (w * w * r * r) / (1 - v) - 0.5;
Mr3 = 3 - 2 / (Mr * Mr) - 0.125 * (1 + 2 * Mv) * (Mw * Mw * Mr * Mr) / (1 - Mv) - 0.5;

{ Mv in [-4.5, -0.3] /\
  Mw in [0.4, 0.9] /\
  Mr in [3.8, 7.8] ->
       |r3 - Mr3| in ? }
