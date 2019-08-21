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

{ Mu in [-100, 100] /\
  Mv in [20, 20000] /\
  MT in [-30, 50]
    -> |r - Mr| in ? }

$ MT, Mv, Mu in 30;
$ MT in 100;
$ Mv in 100;
$ Mu in 100;
