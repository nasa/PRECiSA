@rnd = float<ieee_64,ne>;

t = rnd(Mt);

r rnd= t / (t + 1);
Mr = Mt / (Mt + 1);

{ Mt in [0, 999] -> |r - Mr| in ? }
