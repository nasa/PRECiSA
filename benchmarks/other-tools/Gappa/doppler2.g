@rnd = float<ieee_64,ne>;

Mc1 = 331.4;
Mc2 = 0.6;

c1 = rnd(Mc1);
c2 = rnd(Mc2);

u = rnd(Mu);
v = rnd(Mv);
T = rnd(MT);

t1 rnd= c1 + c2 * T;
r rnd= (-t1 * v) / ((t1 + u) * (t1 + u));
	
Mt1 = Mc1 + Mc2 * MT;
Mr = (-Mt1 * Mv) / ((Mt1 + Mu) * (Mt1 + Mu));

{ Mu in [-125, 125] /\
  Mv in [15, 25000] /\
  MT in [-40, 60]
    -> |r - Mr| in ? }




