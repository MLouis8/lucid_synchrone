open Lazy

(* ---------- Streams ---------- *)
type 'a stream =
  | Cons of 'a option * 'a stream t

let hd = function
  | Cons (Some h, _) -> h
  | Cons (None, _)   -> failwith "head not initialized"

let tl = function Cons (_, t) -> t

let rec stream_1: int stream =
  Cons (Some 1, lazy stream_1)

let rec stream_0: int stream =
  Cons (Some 0, lazy stream_0)

let rec stream_nat (n: int): int stream =
  Cons (Some n, lazy (stream_nat (n+1)))

(* ---------- Clocks ---------- *)
type clock =
  | Cons of bool * clock t

let rec base: clock =
  Cons (true, lazy base)

let rec half: clock =
  Cons (true, lazy (Cons (false, lazy half)))

let clk_hd = function Cons (b, _) -> b

let clk_tl = function Cons (_, t) -> t

let rec complementary (c1: clock) (c2: clock) (n: int): bool = match clk_hd c1 with
  | true when clk_hd c2 = true -> false
  | true -> complementary (force (clk_tl c1)) (force (clk_tl c2)) (n-1)
  | false -> complementary (force (clk_tl c1)) (force (clk_tl c2)) (n-1)

let rec equal (c1: clock) (c2: clock) (n: int): bool =
  if clk_hd c1 = clk_hd c2
    then equal (force (clk_tl c1)) (force (clk_tl c2)) (n-1)
  else false

(* ---------- CStreams ---------- *)
type 'a cstream = {
  value: 'a stream;
  clk: clock;
}

let rec head (s: 'a cstream): 'a =
  match s.clk with
  | Cons (true, _) -> hd s.value
  | Cons (false, c_t) -> head {value=force (tl s.value); clk=force c_t}

let rec tail (s: 'a cstream): 'a cstream =
  match s.clk with
  | Cons (true, c_t) -> {value=force (tl s.value); clk=force c_t}
  | Cons (false, c_t) -> tail {value=force (tl s.value); clk=force c_t}

let rec nth (s: 'a cstream) (n: int): 'a = match n with
  | 0 -> head s
  | _ -> nth (tail s) (n-1)

let rec merge (c: clock) (s1, s2: 'a cstream * 'a cstream): 'a cstream = match c with
  | Cons (true, c_t) -> {
    value=Cons (Some (head s1), lazy (merge (force c_t) (tail s1, tail s2)).value);
    clk=base
  }
  | Cons (false, c_t) ->  {
    value=Cons (Some (head s2), lazy (merge (force c_t) (tail s1, tail s2)).value);
    clk=base
  }

let fby (s1: 'a cstream) (s2: 'a cstream): 'a cstream =
  {value=Cons (Some (head s1), lazy (tail s2).value); clk=s1.clk}

let pre (s: 'a cstream): 'a cstream =
  {value=Cons (None, lazy (tail s).value); clk=s.clk}

(* ---------- Print ---------- *)
let rec print_n_first (s: int cstream) (n: int): unit = match n with
  | 0 -> ()
  | _ -> print_int (head s); print_n_first (tail s) (n-1)

let rec print_n_first (s: int stream) (n: int): unit = match n with
  | 0 -> ()
  | _ -> print_int (hd s); print_n_first (force (tl s)) (n-1)


let main = ();;

main
