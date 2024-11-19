:- dynamic curr_energy/1, curr_attack/1, curr_defense/1, curr_charisma/1.

curr_energy(20).
curr_attack(3).
curr_defense(1).
curr_charisma(0).

game_start_boost :-
    random_between(1, 4, Choice),
    apply_upgrade(Choice).


/* Random stat boost at the beginning of the game */
apply_upgrade(1) :-
    retractall(curr_energy(_)),
    assert(curr_energy(40)).

apply_upgrade(2) :-
    retractall(curr_attack(_)),
    assert(curr_attack(8)).

apply_upgrade(3) :-
    retractall(curr_defense(_)),
    assert(curr_defense(4)).

apply_upgrade(4) :-
    retractall(curr_charisma(_)),
    assert(curr_charisma(1)).


/* Display stats */
stats :-
        curr_energy(E), curr_attack(S),
        curr_defense(D), curr_charisma(C),
        write('Twoje statystyki: '), nl, nl,
        write('Energia: '), write(E), nl,
        write('Siła: '), write(S), nl,
        write('Obrona: '), write(D), nl,
        write('Charyzma: '), write(C), nl.


dean_energy(50).
dean_attack(10).
dean_defense(4).


lecturer_energy(20).
lecturer_attack(5).
lecturer_defense(0).



/* Boosting stats */
boost_stat(Item) :-
    item(Item, _, _, Stat, Value),
    Stat \= none,
    update_stat(Stat, Value).

update_stat(attack, Value) :-
    curr_attack(Current),
    New is Current + Value,
    retract(curr_attack(Current)),
    assert(curr_attack(New)),
    format('Twoja siła wzrosła o ~w!~n', [Value]).

update_stat(defense, Value) :-
    curr_defense(Current),
    New is Current + Value,
    retract(curr_defense(Current)),
    assert(curr_defense(New)),
    format('Twoja obrona wzrosła o ~w!~n', [Value]).

update_stat(energy, Value) :-
    curr_energy(Current),
    New is Current + Value,
    retract(curr_energy(Current)),
    assert(curr_energy(New)),
    format('Twoja energia wzrosła o ~w!~n', [Value]).

update_stat(charisma, Value) :-
    curr_charisma(Current),
    New is Current + Value,
    retract(curr_charisma(Current)),
    assert(curr_charisma(New)),
    format('Twoja charyzma wzrosła o ~w!~n', [Value]).


