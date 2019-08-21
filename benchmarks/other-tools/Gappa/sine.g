@rnd = float<ieee_64,ne>;

x = rnd(Mx);

sine rnd= x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
Msine= Mx - (Mx*Mx*Mx)/6.0 + (Mx*Mx*Mx*Mx*Mx)/120.0 - (Mx*Mx*Mx*Mx*Mx*Mx*Mx)/5040.0;

{ Mx in [-1.57079632679, 1.57079632679]
    -> |sine - Msine| in ? }

