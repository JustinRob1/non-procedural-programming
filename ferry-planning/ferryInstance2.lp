#const steps=20.

car(car1).
car(car2).
car(car3).
car(car4).
car(car5).

location(a).
location(b).
location(c).
location(d).
location(e).
location(f).

at(car1,a,0).
at(car2,a,0).
at(car3,a,0).
at(car4,a,0).
at(car5,a,0).

at(ferry,f,0).

goal(T):-
    at(car1,b,T),
    at(car2,c,T),
    at(car3,d,T),
    at(car4,e,T),
    at(car5,f,T).
