@rnd = float<ieee_64,ne>;

y = rnd(My);

sqroot rnd= 1.0 + 0.5*y - rnd(0.125)*y*y + rnd(0.0625)*y*y*y - rnd(0.0390625)*y*y*y*y;

Msqroot = 1.0 + 0.5*My - 0.125*My*My + 0.0625*My*My*My - 0.0390625*My*My*My*My;

{ My in [0, 1] ->
       |sqroot - Msqroot| in ?}

