/*Need to add optional items, npcs and location descriptions*/
/*Maybe it would be better to describe directions as left, right, forward and backward*/

/*First floor*/
path(start, e, hall_1_1).
path(hall_1_1, e, hall_1_2).
path(hall_1_2, s, hall_1_3).
path(hall_1_3, w, hall_1_4).

path(start, s, hall_1_5).
path(hall_1_5, s, hall_1_6).
path(hall_1_6, e, hall_1_7).
path(hall_1_7, s, hall_1_8).

path(hall_1_8, e, hall_1_9).
path(hall_1_9, n, hall_1_10).
path(hall_1_10, e, hall_1_11).

path(hall_1_8, w, hall_1_12).
path(hall_1_13, s, hall_1_14).
path(hall_1_14, e, hall_1_15).
path(hall_1_15, e, hall_1_16).
path(hall_1_16, e, hall_1_17).
path(hall_1_17, n, hall_1_18).
path(hall_1_18, e, hall_1_19).

path(hall_1_19, s, hall_1_20).
path(hall_1_20, e, hall_1_21).

path(hall_1_19, n, hall_1_22).

path(hall_1_22, e, hall_1_23).
path(hall_1_23, s, hall_1_24).
path(hall_1_24, e, staircase).

path(hall_1_22, n, hall_1_25).
path(hall_1_25, w, hall_1_26).
path(hall_1_26, n, hall_1_27).
path(hall_1_27, e, hall_1_28).
path(hall_1_28, e, hall_1_29).

path(hall_1_29, s, hall_1_30).
path(hall_1_30, e, hall_1_31).
path(hall_1_31, s, hall_1_32).
path(hall_1_32, e, hall_1_33).
path(hall_1_33, n, hall_1_34).

path(hall_1_29, e, hall_1_35).

path(hall_1_35, s, hall_1_31).

path(hall_1_35, e, hall_1_36).
path(hall_1_36, e, hall_1_37).
path(hall_1_37, e, hall_1_38).
path(hall_1_38, s, hall_1_39).
path(hall_1_39, w, hall_1_40).
path(hall_1_40, s, hall_1_41).
path(hall_1_41, s, hall_1_42).

path(hall_1_42, e, hall_1_43).
path(hall_1_43, n, hall_1_44).

path(hall_1_42, w, hall_1_45).
path(hall_1_45, s, hall_1_46).

path(hall_1_46, w, hall_1_47).

path(hall_1_46, e, hall_1_47).
path(hall_1_47, e, end).

/*Second floor*/
path(staircase, e, hall_2_1).
path(hall_2_1, e, hall_2_2).
path(hall_2_2, s, hall_2_3).

path(hall_2_3, w, hall_2_4).
path(hall_2_4, s, hall_2_5).
path(hall_2_5, s, hall_2_6).

path(hall_2_6, e, hall_2_7).
path(hall_2_7, e, hall_2_8).

path(hall_2_6, s, hall_2_9).
path(hall_2_9, w, hall_2_10).
path(hall_2_10, n, hall_2_11).
path(hall_2_11, n, hall_2_12).
path(hall_2_12, n, hall_2_13).

path(hall_2_3, s, hall_2_14).
path(hall_2_14, e, hall_2_15).

path(hall_2_15, n, hall_2_16).
path(hall_2_16, n, hall_2_17).

path(hall_2_15, e, hall_2_18).

path(hall_2_18, n, hall_2_19).
path(hall_2_19, n, hall_2_20).

path(hall_2_18, s, hall_2_21).
path(hall_2_21, s, hall_2_22).
path(hall_2_22, w, hall_2_23).
path(hall_2_23, w, hall_2_24).


at(out-of-date_study_regulations).
at(poor_notes).