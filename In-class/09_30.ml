
(*Bool is an enumerated type also as well as character
*)

type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
let isWorkDay (d:weekday) : bool match d with
  |Monday |Tuesday | Wednesday | Thursday | Friday -> true
  | _ -> false

type shower_status = Clean | Aromatic | AttractingFlies | Bachelor

let num_of_Days l = int_of_float (l/0.016)

let shower (len: float) : shower_status =
  match num_of_Days len with
    | 0 | 1 |2 -> Clean
    |3 | 4 -> Aromatic
    | 5 | 6 | 7 ->AttractingFlies
    | _ ->Bachelor
