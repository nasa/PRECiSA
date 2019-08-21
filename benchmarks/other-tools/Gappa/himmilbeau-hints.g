@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);

r rnd= (x1*x1 + x2 - 11)* (x1*x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7);

Mr = (Mx1*Mx1 + Mx2 - 11)* (Mx1*Mx1 + Mx2 - 11) + (Mx1 + Mx2*Mx2 - 7)*(Mx1 + Mx2*Mx2 - 7);

{ Mx1 in [-5, 5] /\ Mx2 in [-5, 5] ->
       |r - Mr| in ? }

$ Mx1 in 100;
$ Mx2 in 100;
