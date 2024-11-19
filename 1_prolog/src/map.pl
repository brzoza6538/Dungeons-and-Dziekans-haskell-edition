/*Need to add optional items, npcs and location descriptions*/
/*Maybe it would be better to describe directions as left, right, forward and backward*/
:- use_module(library(lists)).
:- dynamic alt_info/2.

reverse_dir(n, s).
reverse_dir(s, n).
reverse_dir(w, e).
reverse_dir(e, w).
reverse_dir(up, down).
reverse_dir(down, up).

/*First floor*/
path_to(start, e, hall1_1_b).
path_to(hall1_1_b, e, hall1_1_c).
path_to(hall1_1_c, s, hall1_2_c).
path_to(hall1_2_c, w, hall1_2_b).

path_to(start, s, hall1_2_a).
path_to(hall1_2_a, s, hall1_3_a).
path_to(hall1_3_a, e, hall1_3_b).
path_to(hall1_3_b, s, hall1_4_b).

path_to(hall1_4_b, e, hall1_4_c).
path_to(hall1_4_c, n, hall1_3_c).
path_to(hall1_3_c, e, hall1_3_d).

path_to(hall1_4_b, w, hall1_4_a).
path_to(hall1_4_a, s, hall1_5_a).
path_to(hall1_5_a, e, hall1_5_b).
path_to(hall1_5_b, e, hall1_5_c).
path_to(hall1_5_c, e, hall1_5_d).
path_to(hall1_5_d, n, hall1_4_d).
path_to(hall1_4_d, e, hall1_4_e).

path_to(hall1_4_e, s, hall1_5_e).
path_to(hall1_5_e, e, hall1_5_f).

path_to(hall1_4_e, n, hall1_3_e).

path_to(hall1_3_e, e, hall1_3_f).
path_to(hall1_3_f, s, hall1_4_f).
path_to(hall1_4_f, e, staircase_1).

path_to(staircase_1, up, staircase_2).

path_to(hall1_3_e, n, hall1_2_e).
path_to(hall1_2_e, w, hall1_2_d).
path_to(hall1_2_d, n, hall1_1_d).
path_to(hall1_1_d, e, hall1_1_e).
path_to(hall1_1_e, e, hall1_1_f).

path_to(hall1_1_f, s, hall1_2_f).
path_to(hall1_2_f, e, hall1_2_g).
path_to(hall1_2_g, s, hall1_3_g).
path_to(hall1_3_g, e, hall1_3_h).
path_to(hall1_3_h, n, hall1_2_h).

path_to(hall1_1_f, e, hall1_1_g).

path_to(hall1_1_g, s, hall1_2_g).

path_to(hall1_1_g, e, hall1_1_h).
path_to(hall1_1_h, e, hall1_1_i).
path_to(hall1_1_i, e, hall1_1_j).
path_to(hall1_1_j, s, hall1_2_j).
path_to(hall1_2_j, w, hall1_2_i).
path_to(hall1_2_i, s, hall1_3_i).
path_to(hall1_3_i, s, hall1_4_i).

path_to(hall1_4_i, e, hall1_4_j).
path_to(hall1_4_j, n, hall1_3_j).

path_to(hall1_4_i, w, hall1_4_h).
path_to(hall1_4_h, s, hall1_5_h).

path_to(hall1_5_h, w, hall1_5_h).

path_to(hall1_5_h, e, hall1_5_h).
path_to(hall1_5_i, e, end).

/*Second floor*/
path_to(staircase_2, e, hall2_1_b).
path_to(hall2_1_b, e, hall2_1_c).
path_to(hall2_1_c, s, hall2_2_c).

path_to(hall2_2_c, w, hall2_2_b).
path_to(hall2_2_b, s, hall2_3_b).
path_to(hall2_3_b, s, hall2_4_b).

path_to(hall2_4_b, e, hall2_4_c).
path_to(hall2_4_c, e, hall2_4_d).

path_to(hall2_4_b, s, hall2_5_b).
path_to(hall2_5_b, w, hall2_5_a).
path_to(hall2_5_a, n, hall2_4_a).
path_to(hall2_4_a, n, hall2_3_a).
path_to(hall2_3_a, n, hall2_2_a).

path_to(hall2_2_c, s, hall2_3_c).
path_to(hall2_3_c, e, hall2_3_d).

path_to(hall2_3_d, n, hall2_2_d).
path_to(hall2_2_d, n, hall2_1_d).

path_to(hall2_3_d, e, hall2_3_e).

path_to(hall2_3_e, n, hall2_2_e).
path_to(hall2_2_e, n, hall2_1_e).

path_to(hall2_3_e, s, hall2_4_e).
path_to(hall2_4_e, s, hall2_5_e).
path_to(hall2_5_e, w, hall2_5_d).
path_to(hall2_5_d, w, hall2_5_c).

/* Making paths bidirectional */
path(FROM, DIR, TO) :-
	path_to(FROM, DIR, TO).
path(FROM, DIR, TO) :-
	reverse_dir(DIR, REVERSE_DIR),
	path_to(TO, REVERSE_DIR, FROM).


/* Alternative position descriptions */
alt_info(start, "Stoisz przed drzwiami dziekanatu.").


object_at(nieaktualny_regulamin_studiow, hall1_2_b).
object_at(poor_notes, hall1_3_d).

choose_random_locations :-
	Potential_locations_1 = [hall1_3_e, hall1_2_g, hall1_3_j, hall1_5_g],
	random_select(Profesora_1_location, Potential_locations_1, Other_locations_1_1),
	random_select(Profesora_2_location, Other_locations_1_1, Other_locations_1_2),
	assert(npc_at(profesora, Profesora_1_location)),
	assert(npc_at(profesora, Profesora_2_location)),
	forall(member(Location, Other_locations_1_2), (assert(npc_at(automat, Location)))),
	Potential_locations_2 = [hall2_4_d, hall2_2_a, hall2_1_d, hall2_1_e, hall2_5_c],
	random_select(Jackpot_location, Potential_locations_2, Other_locations_2),
	assert(object_at(zloty_strzal, Jackpot_location)),
	forall(member(Location, Other_locations_2), (assert(npc_at(dziekana, Location)))).