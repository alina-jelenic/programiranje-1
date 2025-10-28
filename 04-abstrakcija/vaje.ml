(*----------------------------------------------------------------------------*
 # Abstrakcija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Naravna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo `NAT`, ki določa strukturo naravnih števil. Ima osnovni
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov `int` tip. Opomba: Funkcije za
 pretvarjanje ponavadi poimenujemo `to_int` and `of_int`, tako da skupaj z
 imenom modula dobimo ime `NAT.of_int`, ki nam pove, da pridobivamo naravno
 število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq  : t -> t -> bool
  val zero : t
  (* Dodajte manjkajoče! *)
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_int : t -> int 
  val of_int : int -> t 
end

(*----------------------------------------------------------------------------*
 Napišite implementacijo modula `Nat_int`, ki zgradi modul s signaturo `NAT`,
 kjer kot osnovni tip uporablja OCamlov tip `int`. Namig: dokler ne
 implementirate vse funkcij v `Nat_int`, se bo OCaml pritoževal. Temu se lahko
 izognete tako, da funkcije, ki še niso napisane nadomestite z `failwith
 "later"`, vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = x = y
  let zero = 0
  (* Dodajte manjkajoče! *)
  let one = 1
  let ( + ) x y = x + y
  let ( - ) x y = if x < y then 0 else x - y
  let ( * ) x y = x * y
  let to_int n = n
  let of_int n = n
end

(*----------------------------------------------------------------------------*
 Napišite implementacijo `NAT`, ki temelji na [Peanovih
 aksiomih](https://en.wikipedia.org/wiki/Peano_axioms). Osnovni tip modula
 definirajte kot naštevni tip, ki vsebuje konstruktor za ničlo in konstruktor za
 naslednika nekega naravnega števila. Večino funkcij lahko implementirate s
 pomočjo rekurzije. Naprimer, enakost števil `k` in `l` določimo s hkratno
 rekurzijo na `k` in `l`, kjer je osnoven primer `Zero = Zero`.
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = 
  | Zero
  | Suc of t
  (* To morate spremeniti! *)
  let rec eq x y = 
    match x, y with
    | Zero, Zero -> true
    | Suc a, Suc b -> eq a b
    | _, _ -> false
  let zero = Zero (* To morate spremeniti! *)
  (* Dodajte manjkajoče! *)
  let one = Suc Zero
  let rec ( + ) x y = 
    match x with
    | Zero -> y
    | Suc a -> Suc (a + y)
  let rec ( - ) x y = 
    match x, y with
    | x, Zero -> x
    | Suc a, Suc b -> a - b
    | Zero, _ -> Zero
  let rec ( * ) x y =
    match x with
    | Zero -> Zero
    | Suc a -> y + (a * y)
  let rec to_int n = 
    match n with 
    | Zero -> 0
    | Suc a -> Int.add 1 (to_int a)
  let rec of_int n = 
    match n with
    | 0 -> Zero
    | n -> Suc (of_int (Int.sub n 1))

end

(*----------------------------------------------------------------------------*
 Ocaml omogoča sestavljanje modulov iz drugih modulov z uporabo [funktorjev]
 (https://ocaml.org/docs/functors). 
 Funktor je tako preslikava med moduli, zapišemo jo v obliki modula, ki kot
 argumente sprejme druge module. Tako uporabi zapis naslednjo strukturo
 `module Ime_funktorja (Ime_modula : SIG1) : SIG2 = struct ... end`.

 Modul za računanje z naravnimi števili ima signaturo `CALC` (vanjo lahko 
 dodate še kakšne funkcije). Module s to signaturo bomo zgradili z uporabo 
 funktorja `Nat_calculations`, ki sprejme argument modula s signaturo `NAT` 
 in računal z operacijami, ki jih ima ta signatura. 
 Napišite funkciji fakultete in vsote prvih 100 naravnih števil, ki jih signatura 
 `CALC` zahteva.
[*----------------------------------------------------------------------------*)
module type CALC = sig
  type t

  val factorial : t -> t
  val sum_100 : t
end

module Nat_calculations (N: NAT) : CALC with type t := N.t = struct
  let rec factorial n = (* To morate spremeniti! *)
    if N.eq n N.zero then N.one
    else N.( * ) n (factorial (N.( - ) n N.one))

  let sum_100 = (* To morate spremeniti! *)
    let rec aux n acc = 
      if N.eq n N.zero then acc
      else aux (N.(-) n N.one) (N.(+) n acc)
    in
    aux (N.of_int 100) N.zero
end

(*----------------------------------------------------------------------------*
 Z moduli funktorja `Nat_calculations` lahko sedaj preverimo pravilnost
 implementacij `Nat_int` in `Nat_peano`. Definirajte modula `Nat_int_calc` in
 `Nat_peano_calc`, ki sta aplikaciji funktorja `Nat_calculations` na argumentih
 `Nat_int` in `Nat_peano`. Nato za oba primera izračunajte vsoti prvih 100 števil
  in fakulteto 5.
[*----------------------------------------------------------------------------*)

module Nat_int_calc = Nat_calculations (Nat_int)
module Nat_peano_calc = Nat_calculations (Nat_peano)

let sum_100_int = Nat_int.to_int (Nat_int_calc.sum_100)
let sum_100_peano = Nat_peano.to_int (Nat_peano_calc.sum_100)
let fact_5_int = Nat_int.to_int (Nat_int_calc.factorial (Nat_int.of_int 5))
let fact_5_peano = Nat_peano.to_int (Nat_peano_calc.factorial (Nat_peano.of_int 5))

(* val sum_100_int : int = 5050 *)
(* val sum_100_peano : int = 5050 *)
(* val fact_5_int : int = 120 *)
(* val fact_5_peano : int = 120 *)

(*----------------------------------------------------------------------------*
 Funktor lahko sprejme tudi več modulov, njegova končna signatura pa je poljubna,
 torej je lahko enaka signaturi modulov, ki ju sprejme kot argumenta.
 
 Napišite funktor `Nat_pair`, ki sprejme dva modula s signaturo `NAT` in vrne
 modul s signaturo `NAT`. Osnovni tip definiranega modula naj bo par števil 
 tipov modulov iz argumentov. Računske operacije naj delujejo po komponentah.
 Pretvorjanje iz in v `int` pa definirajte poljubno.
[*----------------------------------------------------------------------------*)

 module Nat_pair (A: NAT) (B: NAT) : NAT = struct
  type t = A.t * B.t

  let eq (x1, x2) (y1, y2) = (A.eq x1 y1 && B.eq x2 y2)
  let zero = (A.zero, B.zero)
  (* Dodajte manjkajoče! *)
  let one = (A.one, B.one)
  let ( + ) (x1, x2) (y1, y2) = (A.(+) x1 y1, B.(+) x2 y2)

  let ( - ) (x1, x2) (y1, y2) = (A.(-) x1 y1, B.(-) x2 y2)

  let ( * ) (x1, x2) (y1, y2) = (A.( * ) x1 y1, B.( * ) x2 y2)

  let to_int (a, b) = Int.add (A.to_int a) (B.to_int b)

  let of_int n = 
    let half = n / 2 in
    (A.of_int half, B.of_int (Int.sub n half))

end 

module Nat_pair_int_peano = Nat_pair (Nat_int) (Nat_peano)
(* Poskusite narediti nekaj testnih računov. *)
(*???*)

(*----------------------------------------------------------------------------*
 ## Kompleksna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 > Once upon a time, there was a university with a peculiar tenure
 > policy. All faculty were tenured, and could only be dismissed for
 > moral turpitude. What was peculiar was the definition of moral
 > turpitude: making a false statement in class. Needless to say, the
 > university did not teach computer science. However, it had a renowned
 > department of mathematics.
 >
 > One Semester, there was such a large enrollment in complex variables
 > that two sections were scheduled. In one section, Professor Descartes
 > announced that a complex number was an ordered pair of reals, and that
 > two complex numbers were equal when their corresponding components
 > were equal. He went on to explain how to convert reals into complex
 > numbers, what "i" was, how to add, multiply, and conjugate complex
 > numbers, and how to find their magnitude.
 >
 > In the other section, Professor Bessel announced that a complex number
 > was an ordered pair of reals the first of which was nonnegative, and
 > that two complex numbers were equal if their first components were
 > equal and either the first components were zero or the second
 > components differed by a multiple of 2π. He then told an entirely
 > different story about converting reals, "i", addition, multiplication,
 > conjugation, and magnitude.
 >
 > Then, after their first classes, an unfortunate mistake in the
 > registrar's office caused the two sections to be interchanged. Despite
 > this, neither Descartes nor Bessel ever committed moral turpitude,
 > even though each was judged by the other's definitions. The reason was
 > that they both had an intuitive understanding of type. Having defined
 > complex numbers and the primitive operations upon them, thereafter
 > they spoke at a level of abstraction that encompassed both of their
 > definitions.
 >
 > The moral of this fable is that: Type structure is a syntactic
 > discipline for enforcing levels of abstraction.
 >
 > John C. Reynolds, _Types, Abstraction, and Parametric Polymorphism_, IFIP83
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo modula kompleksnih števil. Potrebujemo osnovni tip, test
 enakosti, ničlo, enko, imaginarno konstanto i, negacijo, konjugacijo,
 seštevanje in množenje.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  (* Dodajte manjkajoče! *)
  val nic : t
  val ena : t
  val imaginarno_i : t
  val negacija : t -> t
  val konjugacija : t -> t
  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
end

(*----------------------------------------------------------------------------*
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x = y
  (* Dodajte manjkajoče! *)
  let nic = {re = 0.; im = 0.}
  let ena = {re = 1.; im = 0.}
  let imaginarno_i = {re = 0.; im = 1.}
  let negacija x = {re = -. x.re; im = -. x.im}
  let konjugacija x = { x with im = -. x.im }
  let ( ++ ) x y = {re = x.re +. y.re; im = x.im +. y.im}
  let ( ** ) x y = {re = (x.re *. y.re) -. (x.im *. y.im); im =  (x.re *. y.im) +. (x.im *. y.re)}

end

(*----------------------------------------------------------------------------*
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
 Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
 (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.
  let normalize_arg arg = if arg >= 2. *. pi then arg -. (2. *. pi) else arg
  let eq x y = x = y
  (* Dodajte manjkajoče! *)
  let nic = {magn = 0.; arg = 0.}
  let ena = {magn = 1.; arg = 0.}
  let imaginarno_i = {magn = 1.; arg = pi /. 2.}
  let negacija x = { x with arg = normalize_arg (x.arg +. pi) }
  let konjugacija x = { x with arg = (2. *. pi) -. x.arg}
  let ( ++ ) x y = failwith "later"
  let ( ** ) x y = {magn = x.magn *. y.magn; arg = normalize_arg (y.arg +. x.arg)}

end
