@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);

t rnd= (3*x1*x1 + 2*x2 - x1);
r rnd=  x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));

Mt = (3*Mx1*Mx1 + 2*Mx2 - Mx1);
Mr = Mx1 + ((2*Mx1*(Mt/(Mx1*Mx1 + 1))*
    (Mt/(Mx1*Mx1 + 1) - 3) + Mx1*Mx1*(4*(Mt/(Mx1*Mx1 + 1))-6))*
    (Mx1*Mx1 + 1) + 3*Mx1*Mx1*(Mt/(Mx1*Mx1 + 1)) + Mx1*Mx1*Mx1 + Mx1 +
    3*((3*Mx1*Mx1 + 2*Mx2 -Mx1)/(Mx1*Mx1 + 1)));

{ Mx1 in [-5, 5] /\
  Mx2 in [-20, 5]
    -> |r - Mr| in ? }

$ Mx1;
$ Mx2;


