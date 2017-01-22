
type party =

    { 
      mutable id         : char;
      mutable votes   	 : float;
      mutable seats      : int;
      mutable divisor    : int;
    }

;;

(*
	data -> receives and verifies if the data inputed by the user is valid.
*)

let rec data value min=

	try
		value := read_int();
		if !value < min then
			begin
			print_string "Invalid value!\n";
			data value min;
			end
		else
			!value
	with
		|_ -> 	print_string "No value inputed!\n";
		      	data value min;
;;

(*
	initialize_array -> asks the user to input the number of votes for each party 
						,storing the data in 2 arrays
*)

let initialize_array parties_hondt parties_saint =
	
	for i = 0 to (Array.length parties_hondt)-1 do 
		Printf.printf "Input the votes for party %c:\n" parties_hondt.(i).id;
	  	parties_hondt.(i).votes<- float_of_int (data (ref 0) 0);	
		parties_saint.(i).votes<- parties_hondt.(i).votes;
	done

;;
(*
	print-> prints parties Ids and seats obtained
*)

let print party = 

	for i = 0 to (Array.length party)-1 do
		Printf.printf "party:  %c	" party.(i).id;
		Printf.printf "seats: %d\n" party.(i).seats;
	done
;;
(*
	findMax -> find the party with the most votes
			   If 2 parties have the same number of votes then the
			   party with the least seats is chosen
*)

let rec findMax party max i=

	if i < (Array.length party) then
		(
		if party.(max).votes< party.(i).votes then 
			findMax party i (i+1)
		else if party.(max).votes= party.(i).votes&& i!=max then
			(
			if party.(max).seats >= party.(i).seats then 
				findMax party i (i+1)
			else
				findMax party max (i+1)
			)
		else
			findMax party max (i+1)
		)
	else
		max
;;
(*
	hondt -> applies both hondt and saint-langue formulas.
			 seats are given to the parties with most votes.
	
*)
let rec hondt party seats methodChosen =

	if seats > 0 then
		(
		let max = findMax party 0 0 in 
		party.(max).seats <- party.(max).seats + 1;
		party.(max).votes<- party.(max).votes *. float_of_int party.(max).divisor;
		if methodChosen = 0 then
			(
				party.(max).divisor <- party.(max).seats + 1;
				party.(max).votes<- party.(max).votes/. float_of_int party.(max).divisor;
				hondt party (seats-1) 0;
			)	
			else	
			(
				party.(max).divisor <- (party.(max).seats * 2) + 1;
				party.(max).votes<- party.(max).votes/. float_of_int party.(max).divisor;
				hondt party (seats-1) 1;
			)	
		)
	else		
		if methodChosen = 0 then
		(
		print_string "D'Hondt method";
		print_string "\n***************************\n";
		print party;
		print_string "***************************\n";
		)
		else
		(
		print_string "Sainte-Laguë method";
		print_string "\n***************************\n";
		print party;
		print_string "***************************\n";
		)
;;
(*
	difference -> prints the difference between the 2 methods
*)
let rec difference parties_hondt parties_saint parties =
	print_string "Difference from Sainte-Laguë method perspective:\n"; 
	let dif = Array.init parties(fun _ -> 0) in 
	for i = 0 to (Array.length parties_hondt)-1 do 
	dif.(i) <- parties_saint.(i).seats;
	dif.(i) <- dif.(i) - parties_hondt.(i).seats;
	if dif.(i) = 0 then
		(
		Printf.printf "party %c: same\n" parties_hondt.(i).id;
		)
	else
		(
		if dif.(i) > 0 then
		(
			Printf.printf "party %c: +%d seat(-s)\n" 
				parties_hondt.(i).id dif.(i);
		)
		else
		(
			Printf.printf "party %c: %d seat(-s)\n" 
				parties_hondt.(i).id dif.(i);
		)
		)
	done
;;


let main() =
	print_string "Input the number of parties:\n";
	let parties = data (ref 0) 2 in
	print_string "Input the number of seats to be contested:\n";
	let seats = data (ref 0) 1 in
	let parties_hondt = Array.init parties(fun i ->  {id= char_of_int (i+65); votes= 0.0;seats = 0;divisor = 1})  in
	let parties_saint = Array.init parties(fun i ->  {id= char_of_int (i+65); votes= 0.0;seats = 0;divisor = 1})  in
	initialize_array parties_hondt parties_saint;
	hondt parties_hondt seats 0;
	hondt parties_saint seats 1;
	difference parties_hondt parties_saint parties;
;;

main()

;;
