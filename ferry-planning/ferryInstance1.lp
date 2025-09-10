#const steps=18.

car(car1).
car(car2).
car(car3).
car(car4).

location(a).
location(b).
location(c).
location(d).
location(e).

at(car1,a,0).
at(car2,a,0).
at(car3,a,0).
at(car4,a,0).

at(ferry,e,0).

goal(T):-
    at(car1,b,T),
    at(car2,c,T),
    at(car3,d,T),
    at(car4,e,T).
