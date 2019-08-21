@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);
x3 = rnd(Mx3);

r1 rnd=  -x1*x2 - 2*x2*x3 - x1 - x3;
Mr1 =  -Mx1*Mx2 - 2*Mx2*Mx3 - Mx1 - Mx3;

{ Mx1 in [-15, 15] /\
  Mx2 in [-15, 15] /\
  Mx3 in [-15, 15] 
    -> |r1 - Mr1| in ? }

$ Mx1;
$ Mx2;
$ Mx3;

