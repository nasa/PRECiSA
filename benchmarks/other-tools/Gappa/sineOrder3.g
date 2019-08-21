@rnd = float<ieee_64,ne>;

z = rnd(Mz);

sineOrder3 rnd= rnd(0.954929658551372) * z -  rnd(0.12900613773279798)*(z*z*z);

MsineOrder3 = 0.954929658551372 * Mz -  0.12900613773279798*(Mz*Mz*Mz);

{ Mz in [-2, 2] ->
       |sineOrder3 - MsineOrder3| in ? }
