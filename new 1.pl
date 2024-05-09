week_schedule([],_,_,[]).
week_schedule(WeekSlots,TAs,DayMax,[DaySched|Rest]) :-
WeekSlots=[H|Wee],
day_schedule(H,TAs,RemTAs,DaySched),
max_slots_per_day(DaySched,DayMax),
week_schedule(Wee,RemTAs,DayMax,Rest).


day_schedule([],X,X,[]).
day_schedule(DaySlots,TAs,RemTAs,[Assignment|Rest]):-
DaySlots=[H|Tay],
slot_assignment(H,TAs,RemTAs1,Assignment),
day_schedule(Tay,RemTAs1,RemTAs,Rest).



max_slots_per_day(DaySched,Max):-
	flatten(DaySched,L),
	remove_duplicates(L,List),
	max_slots_per_day_helper(L,Max,List).
		

max_slots_per_day_helper(DaySched,Max,[H2|T2]):-
	check_occ(H2,DaySched,0,R),
	R=< Max,
	max_slots_per_day_helper(DaySched,Max,T2).
	max_slots_per_day_helper(,,[]).

check_occ(X,[X|T],Acc,R):-
	AccN is Acc+1,
	check_occ(X,T,AccN,R).
		
check_occ(X,[H|T],Acc,R):-
	X\= H,
	check_occ(X,T,Acc,R).

check_occ(_,[],Acc,Acc).			
remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
	 member(H, T),
	 remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
	  \+member(H, T),
	  remove_duplicates( T, T1).
	


slot_assignment(0, A,A, []).
slot_assignment(LabsNum, [ta(Name,Load)|T], RemTAs, [Name|Assignment]):-
LabsNum>0,
LabsNum1 is LabsNum -1, 
ta_slot_assignment([ta(Name, Load)|T],R, Name),
R = [Head|T],
RemTAs = [Head | TRem],
slot_assignment(LabsNum1, T, TRem, Assignment).
slot_assignment(LabsNum, [H|T], [H|Tail], Result):-
LabsNum>0,
slot_assignment(LabsNum, T, Tail, Result).

ta_slot_assignment([], [], _).

ta_slot_assignment([ta(Name, Load) | TAs], [ta(Name, Load1) | RemTAs], Name) :-
    Load > 0,
    Load1 is Load - 1,
    ta_slot_assignment(TAs, RemTAs, Name).

ta_slot_assignment([TA | TAs], [TA | RemTAs], Name) :-
    TA = ta(Name1, _),
    Name \= Name1,
    ta_slot_assignment(TAs, RemTAs, Name).