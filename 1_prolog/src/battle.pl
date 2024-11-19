:- ensure_loaded('./stats.pl').
:- ensure_loaded('./endings.pl').


/* NPC stats - NPC, energy, attack, defense */
npc_stats(dean, 50, 10, 4).
npc_stats(lecturer, 20, 5, 0).
npc_stats(caretaker, 20, 3, 3).


start_battle(NPC) :-
    write('Rozpoczyna się walka z '), write(NPC), write('!'), nl,
    npc_stats(NPC, NPC_Energy, NPC_Attack, NPC_Defense),
    curr_energy(PlayerEnergy),
    battle_loop(PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC).


battle_loop(PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC) :-
    ( PlayerEnergy =< 0 ->
        write('Przegrałeś walkę.'), nl,
        finish_loser, !;
      NPC_Energy =< 0 ->
        write('Pokonałeś '), write(NPC), write('!'), nl, !;
      write('Twoja energia: '), write(PlayerEnergy), nl,
      write('Energia przeciwnika: '), write(NPC_Energy), nl,
      write('Co chcesz zrobić?'), nl,
      write('1. Atakuj'), nl,
      write('2. Czekaj i regeneruj energię'), nl,
      nl, write('Wybór: '),
      read(Choice),
      handle_choice(Choice, PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC)
    ).


handle_choice(1, PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC) :-
    player_attack(NPC_Energy, NewNPC_Energy, NPC_Defense),
    ( NewNPC_Energy =< 0 ->
        write('Pokonałeś '), write(NPC), write('!'), nl, !;
      npc_attack(PlayerEnergy, NewPlayerEnergy, NPC_Attack),
      battle_loop(NewPlayerEnergy, NewNPC_Energy, NPC_Attack, NPC_Defense, NPC)
    ).

handle_choice(2, PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC) :-
    EnergyRegen is 3,
    energy_cap(Cap),
    NewPlayerEnergy is min(PlayerEnergy + EnergyRegen, Cap),
    Regained is NewPlayerEnergy - PlayerEnergy,
    write('Regenerujesz '), write(Regained), write(' punktów energii.'), nl,
    npc_attack(NewPlayerEnergy, FinalPlayerEnergy, NPC_Attack),
    battle_loop(FinalPlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC).

handle_choice(_, PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC) :-
    write('Niepoprawna opcja. Spróbuj ponownie.'), nl,
    battle_loop(PlayerEnergy, NPC_Energy, NPC_Attack, NPC_Defense, NPC).


player_attack(NPC_Energy, NewNPC_Energy, NPC_Defense) :-
    curr_attack(PlayerAttack),
    random_between(1, 6, Roll),
    Damage is max(PlayerAttack + Roll - NPC_Defense, 0),
    NewNPC_Energy is max(NPC_Energy - Damage, 0),
    write('Zadajesz '), write(Damage), write(' obrażeń przeciwnikowi.'), nl.


npc_attack(PlayerEnergy, NewPlayerEnergy, NPC_Attack) :-
    curr_defense(PlayerDefense),
    random_between(1, 6, Roll),
    Damage is max(NPC_Attack + Roll - PlayerDefense, 0),
    NewPlayerEnergy is max(PlayerEnergy - Damage, 0),
    write('Przeciwnik zadaje ci '), write(Damage), write(' obrażeń.'), nl.
