/*Need to add optional items, npcs and location descriptions*/
/*Maybe it would be better to describe directions as left, right, forward and backward*/
:- use_module(library(lists)).

reverse_dir(n, s).
reverse_dir(s, n).
reverse_dir(w, e).
reverse_dir(e, w).

/*First floor*/
path_to(start, e, hall_1_1).
path_to(hall_1_1, e, hall_1_2).
path_to(hall_1_2, s, hall_1_3).
path_to(hall_1_3, w, hall_1_4).

path_to(start, s, hall_1_5).
path_to(hall_1_5, s, hall_1_6).
path_to(hall_1_6, e, hall_1_7).
path_to(hall_1_7, s, hall_1_8).

path_to(hall_1_8, e, hall_1_9).
path_to(hall_1_9, n, hall_1_10).
path_to(hall_1_10, e, hall_1_11).

path_to(hall_1_8, w, hall_1_12).
path_to(hall_1_13, s, hall_1_14).
path_to(hall_1_14, e, hall_1_15).
path_to(hall_1_15, e, hall_1_16).
path_to(hall_1_16, e, hall_1_17).
path_to(hall_1_17, n, hall_1_18).
path_to(hall_1_18, e, hall_1_19).

path_to(hall_1_19, s, hall_1_20).
path_to(hall_1_20, e, hall_1_21).

path_to(hall_1_19, n, hall_1_22).

path_to(hall_1_22, e, hall_1_23).
path_to(hall_1_23, s, hall_1_24).
path_to(hall_1_24, e, staircase).

path_to(hall_1_22, n, hall_1_25).
path_to(hall_1_25, w, hall_1_26).
path_to(hall_1_26, n, hall_1_27).
path_to(hall_1_27, e, hall_1_28).
path_to(hall_1_28, e, hall_1_29).

path_to(hall_1_29, s, hall_1_30).
path_to(hall_1_30, e, hall_1_31).
path_to(hall_1_31, s, hall_1_32).
path_to(hall_1_32, e, hall_1_33).
path_to(hall_1_33, n, hall_1_34).

path_to(hall_1_29, e, hall_1_35).

path_to(hall_1_35, s, hall_1_31).

path_to(hall_1_35, e, hall_1_36).
path_to(hall_1_36, e, hall_1_37).
path_to(hall_1_37, e, hall_1_38).
path_to(hall_1_38, s, hall_1_39).
path_to(hall_1_39, w, hall_1_40).
path_to(hall_1_40, s, hall_1_41).
path_to(hall_1_41, s, hall_1_42).

path_to(hall_1_42, e, hall_1_43).
path_to(hall_1_43, n, hall_1_44).

path_to(hall_1_42, w, hall_1_45).
path_to(hall_1_45, s, hall_1_46).

path_to(hall_1_46, w, hall_1_47).

path_to(hall_1_46, e, hall_1_47).
path_to(hall_1_47, e, end).

/*Second floor*/
path_to(staircase, e, hall_2_1).
path_to(hall_2_1, e, hall_2_2).
path_to(hall_2_2, s, hall_2_3).

path_to(hall_2_3, w, hall_2_4).
path_to(hall_2_4, s, hall_2_5).
path_to(hall_2_5, s, hall_2_6).

path_to(hall_2_6, e, hall_2_7).
path_to(hall_2_7, e, hall_2_8).

path_to(hall_2_6, s, hall_2_9).
path_to(hall_2_9, w, hall_2_10).
path_to(hall_2_10, n, hall_2_11).
path_to(hall_2_11, n, hall_2_12).
path_to(hall_2_12, n, hall_2_13).

path_to(hall_2_3, s, hall_2_14).
path_to(hall_2_14, e, hall_2_15).

path_to(hall_2_15, n, hall_2_16).
path_to(hall_2_16, n, hall_2_17).

path_to(hall_2_15, e, hall_2_18).

path_to(hall_2_18, n, hall_2_19).
path_to(hall_2_19, n, hall_2_20).

path_to(hall_2_18, s, hall_2_21).
path_to(hall_2_21, s, hall_2_22).
path_to(hall_2_22, w, hall_2_23).
path_to(hall_2_23, w, hall_2_24).

/* Making paths bidirectional */
path(FROM, DIR, TO) :-
	path_to(FROM, DIR, TO).
path(FROM, DIR, TO) :-
	reverse_dir(DIR, REVERSE_DIR),
	path_to(TO, REVERSE_DIR, FROM).


object_at(out-of-date_study_regulations, hall_1_4).
object_at(poor_notes, hall_1_11).

choose_random_locations :-
	Potential_locations_1 = [hall_1_22, hall_1_31, hall_1_44, hall_1_47],
	random_select(Professor_1_location, Potential_locations_1, Other_locations_1_1),
	random_select(Professor_2_location, Other_locations_1_1, Other_locations_1_2),
	assert(npc_at(professor, Professor_1_location)),
	assert(npc_at(professor, Professor_2_location)),
	forall(member(Location, Other_locations_1_2), (assert(object_at(vending_machine, Location)))),
	Potential_locations_2 = [hall_2_8, hall_2_13, hall_2_17, hall_2_20, hall_2_24],
	random_select(Jackpot_location, Potential_locations_2, Other_locations_2),
	assert(object_at(jackpot, Jackpot_location)),
	forall(member(Location, Other_locations_2), (assert(npc_at(dean, Location)))).