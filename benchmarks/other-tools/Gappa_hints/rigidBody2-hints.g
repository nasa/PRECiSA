@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);
x3 = rnd(Mx3);

r2 rnd= 2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2;
Mr2 = 2*Mx1*Mx2*Mx3 + 3*Mx3*Mx3 - Mx2*Mx1*Mx2*Mx3 + 3*Mx3*Mx3 - Mx2;

{ Mx1 in [-15, 15] /\
  Mx2 in [-15, 15] /\
  Mx3 in [-15, 15] 
    -> |r2 - Mr2| in ? }

$ Mx1 in 100;
$ Mx2 in 100;
$ Mx3 in 100;
