:- dynamic curr_energy/1, curr_attack/1, curr_defense/1, curr_charisma/1.

curr_energy(20).
curr_attack(3).
curr_defense(1).
curr_charisma(0).
energy_cap(20).

game_start_boost :-
    random_between(1, 4, Choice),
    apply_upgrade(Choice).


/* Random stat boost at the beginning of the game */
apply_upgrade(1) :-
    retractall(curr_energy(_)),
    assert(curr_energy(40)),
    assert(energy_cap(40)).

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


/* Boosting stats */
boost_stat(Item) :-
    item(Item, _, _, Stat, Value),
    Stat \= none,
    update_stat(Stat, Value).

decrease_stat(Item) :-
    item(Item, _, _, Stat, Value),
    Stat \= none,
    update_stat(Stat, -Value).

update_stat(attack, Value) :-
    curr_attack(Current),
    New is Current + Value,
    retract(curr_attack(Current)),
    assert(curr_attack(New)),
    format('Twoja siła zmieniła się o ~w!~n', [Value]).

update_stat(defense, Value) :-
    curr_defense(Current),
    New is Current + Value,
    retract(curr_defense(Current)),
    assert(curr_defense(New)),
    format('Twoja obrona zmieniła się o ~w!~n', [Value]).

update_stat(energy, Value) :-
    curr_energy(Current),
    New is Current + Value,
    retract(curr_energy(Current)),
    assert(curr_energy(New)),
    format('Twoja energia zmieniła się o ~w!~n', [Value]).

update_stat(charisma, Value) :-
    curr_charisma(Current),
    New is Current + Value,
    retract(curr_charisma(Current)),
    assert(curr_charisma(New)),
    format('Twoja charyzma zmieniła się o ~w!~n', [Value]).


