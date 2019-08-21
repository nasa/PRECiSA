@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);
x3 = rnd(Mx3);
x4 = rnd(Mx4);

r1 rnd=  x1 * x4 * (-x1 + x2 + x3 - x4)
                 + x2 * (x1 - x2 + x3 + x4)
		 + x3 * (x1 + x2 - x3 + x4)
		 - x2 * x3 * x4 - x1 * x3 - x1 * x2 - x4;

Mr1 =  Mx1 * Mx4 * (-Mx1 + Mx2 + Mx3 - Mx4)                 + Mx2 * (Mx1 - Mx2 + Mx3 + Mx4)		 + Mx3 * (Mx1 + Mx2 - Mx3 + Mx4)		 - Mx2 * Mx3 * Mx4 - Mx1 * Mx3 - Mx1 * Mx2 - Mx4;

{ 
  Mx1 in [4, 6.36] /\
  Mx2 in [4, 6.36] /\
  Mx3 in [4, 6.36] /\
  Mx4 in [4, 6.36] 
    -> |r1 - Mr1| in ?}


