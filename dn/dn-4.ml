(*----------------------------------------------------------------------------*
  # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ## Slovarji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali iskalna drevesa in implementacijo 
  AVL-dreves za predstavitev množic. V tej nalogi morate s pomočjo AVL-dreves 
  implementirati `slovar`, ki preslika ključe tipa `'k` v vrednosti tipa `'v`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  ### Stroga ureditev

  Za predstavitev slovarja potrebujemo strogo ureditev na tipu ključev.
  Najprej definiramo tip `ureditev`, ki predstavlja možne izide
  primerjave dveh elementov, nato pa še modul `UREJEN_TIP`, s katerim
  lahko primerjamo abstraktne elemente.


[*----------------------------------------------------------------------------*)

type ureditev = Less | Equal | Greater

module type UREJEN_TIP = sig
  type t

  val primerjaj : t -> t -> ureditev
end

module INT_UREJEN_TIP : UREJEN_TIP with type t = int = struct
  type t = int

  let primerjaj x y = if x < y then Less else if x > y then Greater else Equal
end

(*----------------------------------------------------------------------------*
  Sestavite modul `STRING_UREJEN_TIP`, ki implementira `UREJEN_TIP` za tip
  `string`.
[*----------------------------------------------------------------------------*)

module STRING_UREJEN_TIP : UREJEN_TIP with type t = string = struct
  type t = string

  let primerjaj x y = if x < y then Less else if x > y then Greater else Equal
end;;

STRING_UREJEN_TIP.primerjaj "abc" "abd"
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  Za poljuben tip lahko definiramo `razširitev` z najmanjšim in največjim
  elementom. Sestavite parametriziran modul `RAZSIRJEN_UREJEN_TIP`, ki
  sprejme modul, ki implementira signaturo `UREJEN_TIP`, in vrne modul, ki
  implementira signaturo `UREJEN_TIP` za razširjeni tip.
[*----------------------------------------------------------------------------*)

type 'a razsiritev = MinInf | PlusInf | Value of 'a

module RAZSIRJEN_UREJEN_TIP (U : UREJEN_TIP) :
  UREJEN_TIP with type t = U.t razsiritev = struct
  type t = U.t razsiritev

  let primerjaj x y = 
    match x, y with
    | MinInf, MinInf -> Equal
    | MinInf, _ -> Less
    | _, MinInf -> Greater
    | PlusInf, PlusInf -> Equal
    | PlusInf, _ -> Greater
    | _, PlusInf -> Less
    | Value v1, Value v2 -> U.primerjaj v1 v2
end

module LIFTED_INT_UREJEN_TIP = RAZSIRJEN_UREJEN_TIP (INT_UREJEN_TIP);;

LIFTED_INT_UREJEN_TIP.primerjaj MinInf (Value 3)
(* - : ureditev = Less *)

(*----------------------------------------------------------------------------*
  ### AVLSlovar

  Sestavite parametriziran modul `MAKE_SLOVAR`, ki sprejme modul, ki
  implementira `UREJEN_TIP`, in vrne modul s signaturo `SLOVAR`. Vaš slovar
  naj bo implementiran z AVL-drevesi, tako da je vstavljanje in iskanje v
  slovarju v času `O(log n)`.
[*----------------------------------------------------------------------------*)

module type SLOVAR = sig
  type kljuc
  type 'a t

  val prazen : 'a t
  (** Vrne prazen slovar. *)
  val dodaj : kljuc -> 'a -> 'a t -> 'a t
  (** Doda nov par `kljuc`, `vrednost` v slovar. Če ključ v slovarju že obstaja, 
      se njegova vrednost posodobi. *)
  val popravi : kljuc -> ('a option -> 'a option) -> 'a t -> 'a t
  (** Popravi vrednost pod ključem `kljuc` s funkcijo `f`. Če ključ v slovarju
      ne obstaja, se pokliče `f None`, sicer `f (Some vrednost)`. Če je rezultat
      klica `f` enak `None`, se par odstrani iz slovarja, če je rezultat klica 
      `Some v`, se pod ključ `kljuc` zapiše vrednost `v`.*)
  val odstrani : kljuc -> 'a t -> 'a t
  (** Odstrani par s ključem `kljuc` iz slovarja. Če ključa v slovarju ni, naj 
      funkcija vrne prvotni slovar in ne sproži napake. *)
  val velikost : 'a t -> int
  (** Vrne število elementov v slovarju. *)
  val kljuci : 'a t -> kljuc list
  (** Našteje ključe v slovarju v enakem vrstnem redu kot to določa urejenost. *)
  val vrednosti : 'a t -> 'a list
  (** Našteje vrednosti v slovarju v enakem vrstnem redu kot to določa urejenost
      pripadajočih ključev. *)
  val najmanjsi_opt : 'a t -> (kljuc * 'a) option
  (** Vrne najmanjši ključ v slovarju ali `None`, če je slovar prazen. *)
  val najvecji_opt : 'a t -> (kljuc * 'a) option
  (** Vrne največji ključ v slovarju ali `None`, če je slovar prazen. *)
  val poisci_opt : kljuc -> 'a t -> 'a option
  (** Poišče vrednost pod ključem `kljuc`. Če ključ v slovarju ne obstaja,
      vrne `None`. *)
  val iter : (kljuc -> 'a -> unit) -> 'a t -> unit
  (** Izvede funkcijo za vsak par ključ, vrednost v slovarju v enakem vrstnem 
      redu kot ga določa urejenost. *)
  val zlozi : (kljuc -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Zloži slovar z dano funkcijo in začetno vrednostjo. Elementi se obdelajo
      v enakem vrstnem redu kot ga določa urejenost.
  
      Specifično za
      `zlozi f slovar acc = f k_n v_n (... (f k_2 v_2 (f k_1 v_1 acc))...)`
      , kjer so `(k_1, v_1), ..., (k_n, v_n)` pari ključ, vrednost v slovarju 
      urejeni po ključih.
  *)
  val preslikaj : ('a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo. *)
  val preslikaji : (kljuc -> 'a -> 'b) -> 'a t -> 'b t
  (** Preslika vrednosti v slovarju z dano funkcijo, ki poleg vrednosti dobi še
      pripadajoči ključ. *)
  val vsebuje : kljuc -> 'a t -> bool
  (** Preveri, ali slovar vsebuje podan ključ. *)
  val za_vse : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali za vse pare ključ, vrednost v slovarju velja podan pogoj. *)
  val obstaja : (kljuc -> 'a -> bool) -> 'a t -> bool
  (** Preveri, ali obstaja vsaj en par ključ, vrednost v slovarju, ki izpolnjuje
      podan pogoj. *)
  val v_seznam : 'a t -> (kljuc * 'a) list
  (** Pretvori slovar v seznam parov ključ, vrednost v enakem vrstnem redu kot
      to določa urejenost. *)
  val iz_seznama : (kljuc * 'a) list -> 'a t
  (** Ustvari slovar iz seznama parov ključ, vrednost. Če se ključi v seznamu
      ponavljajo, naj enak ključ obdrži zadnjo vrednost. *)
end


module MAKE_SLOVAR (U : UREJEN_TIP) : SLOVAR with type kljuc = U.t = struct
  type kljuc = U.t
  type 'a t = 
    | Prazno
    | Vozlisce of {
        levo : 'a t;
        kljuc : kljuc;
        vrednost : 'a;
        desno : 'a t;
        visina : int;
      }

  (**Pomožne funkcije za rotacije in uravnoteženje AVL dreves*)
  let prazen : 'a t = Prazno
  let je_prazno = function
    | Prazno -> true
    | Vozlisce _ -> false

  let visina = function
    | Prazno -> 0
    | Vozlisce { visina; _ } -> visina

  let ravnotezni_faktor drevo =
    match drevo with
    | Prazno -> 0
    | Vozlisce { levo; desno; _ } -> visina levo - visina desno

  let ustvari_vozlisce levo kljuc vrednost desno =
    Vozlisce {
      levo;
      kljuc;
      vrednost;
      desno;
      visina = 1 + max (visina levo) (visina desno);
    }
    (**Rotacije in uravnoteženje za pravilno zgradbo drevesa po novo dod vozlišču*)
  let rotiraj_desno = function
    | Vozlisce { levo = Vozlisce { levo = a; kljuc = k1; vrednost = v1; desno = b; _ }; 
                 kljuc = k2; vrednost = v2; desno = c; _ } ->
        ustvari_vozlisce a k1 v1 (ustvari_vozlisce b k2 v2 c)
    | drevo -> drevo

  let rotiraj_levo = function
    | Vozlisce { levo = a; kljuc = k1; vrednost = v1; 
                 desno = Vozlisce { levo = b; kljuc = k2; vrednost = v2; desno = c; _ }; _ } ->
        ustvari_vozlisce (ustvari_vozlisce a k1 v1 b) k2 v2 c
    | drevo -> drevo

  let rotiraj_levo_desno = function
    | Vozlisce node ->
        let novo_levo = rotiraj_levo node.levo in
        rotiraj_desno (ustvari_vozlisce novo_levo node.kljuc node.vrednost node.desno)
    | drevo -> drevo

  let rotiraj_desno_levo = function
    | Vozlisce node ->
        let novo_desno = rotiraj_desno node.desno in
        rotiraj_levo (ustvari_vozlisce node.levo node.kljuc node.vrednost novo_desno)
    | drevo -> drevo
  
  let uravnotezi drevo =
  match drevo with
  | Prazno -> Prazno
  | Vozlisce node ->
      let x = ravnotezni_faktor drevo in
      let novo_drevo = 
        if x > 1 then
          if ravnotezni_faktor node.levo >= 0 then
            rotiraj_desno drevo
          else
            rotiraj_levo_desno drevo
        else if x < -1 then
          if ravnotezni_faktor node.desno <= 0 then
            rotiraj_levo drevo
          else
            rotiraj_desno_levo drevo
        else
          drevo
      in
      match novo_drevo with
      | Prazno -> Prazno
      | Vozlisce n -> 
          Vozlisce { n with visina = 1 + max (visina n.levo) (visina n.desno) }
          
  (**Funkcije iz signature*)
  let rec dodaj k v s = 
    match s with
    | Prazno -> ustvari_vozlisce Prazno k v Prazno
    | Vozlisce node -> 
      match U.primerjaj k node.kljuc with
        | Equal ->
            Vozlisce { node with vrednost = v }
        | Less ->
            let novo_levo = dodaj k v node.levo in
            uravnotezi (ustvari_vozlisce novo_levo node.kljuc node.vrednost node.desno)
        | Greater ->
            let novo_desno = dodaj k v node.desno in
            uravnotezi (ustvari_vozlisce node.levo node.kljuc node.vrednost novo_desno)

    let rec odstrani k slovar = 
      let rec odstrani_najmanjsi d =
        match d with
        | Prazno -> failwith "Napaka: odstrani_najmanjsi na praznem drevesu"
        | Vozlisce { levo = Prazno; kljuc; vrednost; desno; _ } ->
          (kljuc, vrednost), uravnotezi desno
        | Vozlisce node ->
          let par, novo_levo = odstrani_najmanjsi node.levo in
          let novo_vozlisce =  ustvari_vozlisce novo_levo node.kljuc node.vrednost node.desno in
          par, uravnotezi novo_vozlisce
      in
      match slovar with
      | Prazno -> Prazno
      | Vozlisce node -> 
        match U.primerjaj k node.kljuc with 
        | Equal ->
          (match node.levo, node.desno with
          | Prazno, Prazno -> Prazno
          | _, Prazno -> uravnotezi node.levo
          | Prazno, _ -> uravnotezi node.desno
          | _, _ -> 
            let (naslednik_kljuc, naslednik_vrednost), novo_desno = odstrani_najmanjsi node.desno
            in
            let novo_vozlisce =
              ustvari_vozlisce node.levo naslednik_kljuc naslednik_vrednost novo_desno
          in
          uravnotezi novo_vozlisce)

        | Less ->
          let novo_levo = odstrani k node.levo in
          uravnotezi (Vozlisce { node with levo = novo_levo }) 
        | Greater ->
          let novo_desno = odstrani k node.desno in
          uravnotezi (Vozlisce {node with desno = novo_desno})

  let rec popravi k f s = 
    match s with
    | Prazno -> 
      let rezultat = f None in
      (match rezultat with
      | None -> Prazno
      | Some x -> dodaj k x s)
    | Vozlisce node -> 
      (match U.primerjaj k node.kljuc with
      | Equal -> 
        let rezultat = f (Some node.vrednost) in 
        (match rezultat with
        | None -> odstrani k s
        | Some x -> Vozlisce {node with vrednost = x})
      | Less -> 
        let novo_levo = popravi k f node.levo in
        uravnotezi (ustvari_vozlisce novo_levo node.kljuc node.vrednost node.desno)
      | Greater ->
        let novo_desno = popravi k f node.desno in
        uravnotezi (ustvari_vozlisce node.levo node.kljuc node.vrednost novo_desno))

  let rec velikost s = 
    match s with
    | Prazno -> 0
    | Vozlisce node -> 1 + velikost node.levo + velikost node.desno
  
  let rec kljuci s = 
    match s with
    | Prazno -> []
    | Vozlisce node -> (kljuci node.levo) @ [node.kljuc] @ (kljuci node.desno)

  let rec vrednosti s = 
    match s with
    | Prazno -> []
    | Vozlisce node -> (vrednosti node.levo) @ [node.vrednost] @ (vrednosti node.desno)

  let rec najmanjsi_opt s = 
    match s with
    | Prazno -> None
    | Vozlisce node ->
      if node.levo = Prazno then Some (node.kljuc, node.vrednost)
      else najmanjsi_opt node.levo

  let rec najvecji_opt s = 
    match s with
    | Prazno -> None
    | Vozlisce node ->
      if node.desno = Prazno then Some (node.kljuc, node.vrednost)
      else najvecji_opt node.desno

  let rec poisci_opt k s = 
    match s with
    | Prazno -> None
    | Vozlisce node -> 
      match U.primerjaj k node.kljuc with
      | Equal -> Some node.vrednost
      | Less -> poisci_opt k node.levo
      | Greater -> poisci_opt k node.desno

  let rec iter f s =
    match s with
    | Prazno -> ()
    | Vozlisce node ->
      iter f node.levo;
      f node.kljuc node.vrednost;
      iter f node.desno

  let rec zlozi f s acc = 
    match s with
    | Prazno -> acc
    | Vozlisce node -> 
      let levi_acc = zlozi f node.levo acc in
      let vozlisce_acc = f node.kljuc node.vrednost levi_acc in
      zlozi f node.desno vozlisce_acc

  let rec preslikaj f s = 
    match s with
    | Prazno -> Prazno
    | Vozlisce node ->
      let novo_levo = preslikaj f node.levo in
      let novo_desno = preslikaj f node.desno in
      ustvari_vozlisce novo_levo node.kljuc (f node.vrednost) novo_desno

  let rec preslikaji f s = 
    match s with
    | Prazno -> Prazno
    | Vozlisce node ->
      let novo_levo = preslikaji f node.levo in
      let novo_desno = preslikaji f node.desno in
      ustvari_vozlisce novo_levo node.kljuc (f node.kljuc node.vrednost) novo_desno

  let rec vsebuje k s = 
    match s with
    | Prazno -> false
    | Vozlisce node ->
      match U.primerjaj k node.kljuc with
      | Equal -> true
      | Less -> vsebuje k node.levo
      | Greater -> vsebuje k node.desno

  let rec za_vse f s = 
    match s with
    | Prazno -> true
    | Vozlisce node -> 
      (f node.kljuc node.vrednost) && (za_vse f node.levo) && (za_vse f node.desno)

  let rec obstaja f s = 
    match s with
    | Prazno -> false
    | Vozlisce node ->
      if f node.kljuc node.vrednost then true
      else obstaja f node.levo || obstaja f node.desno
  let v_seznam s =
    List.rev (zlozi (fun k v acc -> (k, v) :: acc) s [])

  let iz_seznama sez = 
    let rec aux seznam acc = 
      match seznam with
      | [] -> acc
      | (x,y)::xs -> 
        let nov = dodaj x y acc in
        aux xs nov
    in
    aux sez prazen

  
end

module SLOVAR_NIZ = MAKE_SLOVAR (STRING_UREJEN_TIP)

let slovar =
  SLOVAR_NIZ.iz_seznama
    [ ("jabolko", "apple"); ("banana", "banana"); ("cesnja", " cherry") ]
  |> SLOVAR_NIZ.dodaj "datelj" "date"
  |> SLOVAR_NIZ.odstrani "banana"
  |> SLOVAR_NIZ.popravi "cesnja" (function
       | None -> Some "cherry"
       | Some v -> Some ("sour " ^ v))
  |> SLOVAR_NIZ.preslikaj String.length
  |> SLOVAR_NIZ.v_seznam

(*----------------------------------------------------------------------------*
  ## Turingovi stroji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Na predavanjih in vajah smo si ogledali Turingove stroje. Pred vami je
  neučinkovito implementiran Turingov stroj. Vaša naloga je, da implementacijo
  s pomočjo slovarjev izboljšate tako, da bo deloval učinkoviteje.
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

module Tape : TAPE = struct
  type t = { left : char list; right : char list }

  let make str = { left = []; right = str |> String.to_seq |> List.of_seq }

  let print { left; right } =
    List.rev_append left right |> List.to_seq |> String.of_seq |> print_endline;
    print_endline (String.make (List.length left) ' ' ^ "^")

  let read { right } = match right with [] -> ' ' | chr :: _ -> chr

  let move dir { left; right } =
    match (dir, left, right) with
    | Left, ' ' :: left, [] -> { left; right }
    | Left, chr :: left, right -> { left; right = chr :: right }
    | Left, [], right -> { left = []; right = ' ' :: right }
    | Right, [], ' ' :: right -> { left; right }
    | Right, left, chr :: right -> { left = chr :: left; right }
    | Right, left, [] -> { left = ' ' :: left; right = [] }

  let write chr { left; right } =
    match right with
    | [] when chr = ' ' -> { left; right }
    | [] -> { left; right = [ chr ] }
    | _ :: right -> { left; right = chr :: right }
end

let primer_trak =
  Tape.(
    make "ABCDE" |> move Left |> move Left |> move Right |> move Right
    |> move Right |> move Right |> write '!' |> print)

module type MACHINE = sig
  type t

  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
  val run : t -> state -> unit
  val speed_run : t -> state -> unit
end

module MachineNeucinkovito : MACHINE = struct
  type t = {
    initial : state;
    transitions : (state * char * state * char * direction) list;
  }

  let make initial _states = { initial; transitions = [] }
  let initial { initial } = initial

  let add_transition st chr st' chr' dir tm =
    { tm with transitions = (st, chr, st', chr', dir) :: tm.transitions }

  let step tm st tape =
    let chr = Tape.read tape in
    match
      List.find_opt
        (fun (st', chr', _, _, _) -> st = st' && chr = chr')
        tm.transitions
    with
    | None -> None
    | Some (_, _, st', chr', dir) ->
        Some (st', tape |> Tape.write chr' |> Tape.move dir)

  let run tm str =
    let rec step' (st, tape) =
      (Tape.print tape;
      print_endline st;
      match step tm st tape with
      | None -> ()
      | Some config' -> step' config')
    in
    step' (initial tm, Tape.make str)

  let speed_run tm str =
  let rec step' (st, tape) =
    match step tm st tape with
    | None -> Tape.print tape
    | Some config' -> step' config'
  in
  step' (initial tm, Tape.make str)
end

(*----------------------------------------------------------------------------*
  Sestavite modul `MachineUcinkovito`, ki učinkovito implementira signaturo
  `MACHINE` z uporabo slovarja, ki ste ga implementirali pred tem. Na kratko
  analizirajte časovno zahtevnost operacij `add_transition` in `step` v
  primerjavi z neučinkovito implementacijo.

  Namig:  
  Za dodatne točke je časovna zahtevnost iskanja prehoda v funkciji
  `speed_run` z nekaj preprocesiranja konstantna.
[*----------------------------------------------------------------------------*)
module PAR_NIZ_CHAR_UREJEN : UREJEN_TIP with type t = string * char = struct
  type t = string * char

  let primerjaj (s1, c1) (s2, c2) =
    match STRING_UREJEN_TIP.primerjaj s1 s2 with
    | Equal -> if c1 < c2 then Less else if c1 > c2 then Greater else Equal
    | other -> other
end

module SLOVAR_PAR = MAKE_SLOVAR (PAR_NIZ_CHAR_UREJEN)

module MachineUcinkovito : MACHINE = struct
  type transition = state * char * direction
  type t = {
    initial : state;
    transitions : transition SLOVAR_PAR.t;
  }

  let make initial _states = { initial; transitions = SLOVAR_PAR.prazen }

  let initial { initial } = initial

  let add_transition st chr st' chr' dir tm =
    let kljuc = (st, chr) in
    let vrednost = (st', chr', dir) in
    { tm with transitions = SLOVAR_PAR.dodaj kljuc vrednost tm.transitions}

  let step tm st tape =
    let chr = Tape.read tape in
    let k = (st, chr) in
    match
      SLOVAR_PAR.poisci_opt k tm.transitions
    with
    | None -> None
    | Some (st, chr', dir) -> Some (st, tape |> Tape.write chr' |> Tape.move dir)
        
  let run tm str =
    let rec step' (st, tape) =
      (Tape.print tape;
      print_endline st;
      match step tm st tape with
      | None -> ()
      | Some config' -> step' config')
    in
    step' (initial tm, Tape.make str)

  let speed_run tm str =
    let open Hashtbl in
    let h = create (SLOVAR_PAR.velikost tm.transitions + 10) in
    let () = SLOVAR_PAR.iter (fun k v -> add h k v) tm.transitions in
    let step' st tape =
      let chr = Tape.read tape in
      match find_opt h (st, chr) with
      | None -> None
      | Some (st', chr', dir) -> Some (st', tape |> Tape.write chr' |> Tape.move dir)
    in
    let rec loop (st, tape) =
      match step' st tape with
      | None -> Tape.print tape
      | Some x -> loop x
    in
    loop (initial tm, Tape.make str)
end

(*----------------------------------------------------------------------------*
Analiza računske zahtevnosti:
- `add_transition`: Prej je imela računsko zahtevnost O(1), sedaj pa ima 
računsko zahtevnost enako računski zahtevnosti funkcije dodaj (iz slovarjev) 
torej s O(logn).
- `step`: Prej je imela računsko zahtevnost O(n), saj je v najslabšem primeru
morala iti čez cel seznam, sedaj pa ima enako računsko zahtevnost kot 
poisci_opt iz slovarja torej O(logn)
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhodnem nizu prepozna palindrom (iz `0` in
  `1`). Če je na vhodnem nizu palindrom, naj na koncu na traku zapiše `1`,
  sicer `0`.
[*----------------------------------------------------------------------------*)

(*ideja: https://stackoverflow.com/questions/78480329/turing-machine-transition-table-for-checking-a-palindrome*)

let palindrom_stroj : MachineUcinkovito.t =
  let open MachineUcinkovito in
  make "zacni" []
    (* zacni: vzemi prvi ne-blank znak; če je blank → sprejmi *)
  |> add_transition "zacni" '0' "isci_0" ' ' Right
  |> add_transition "zacni" '1' "isci_1" ' ' Right
  |> add_transition "zacni" ' ' "zapis_1" ' ' Right

    (* isci_0: pomik desno do konca niza *)
  |> add_transition "isci_0" '0' "isci_0" '0' Right
  |> add_transition "isci_0" '1' "isci_0" '1' Right
  |> add_transition "isci_0" ' ' "preveri_zadnji_0" ' ' Left

    (* preveri_zadnji_0: preveri zadnji simbol *)
  |> add_transition "preveri_zadnji_0" '0' "vrni_se_levo" ' ' Left     
  |> add_transition "preveri_zadnji_0" '1' "brisi_levo" ' ' Left  
  |> add_transition "preveri_zadnji_0" ' ' "zapis_1" ' ' Right         

    (* isci_1 / preveri_zadnji_1 – analogno za 1 *)
  |> add_transition "isci_1" '0' "isci_1" '0' Right
  |> add_transition "isci_1" '1' "isci_1" '1' Right
  |> add_transition "isci_1" ' ' "preveri_zadnji_1" ' ' Left

  |> add_transition "preveri_zadnji_1" '1' "vrni_se_levo" ' ' Left
  |> add_transition "preveri_zadnji_1" '0' "brisi_levo" ' ' Left
  |> add_transition "preveri_zadnji_1" ' ' "zapis_1" ' ' Right

    (* vrni_se_levo: vrnemo se na levi rob *)
  |> add_transition "vrni_se_levo" '0' "vrni_se_levo" '0' Left
  |> add_transition "vrni_se_levo" '1' "vrni_se_levo" '1' Left
  |> add_transition "vrni_se_levo" ' ' "zacni" ' ' Right

    (* brisi_levo: pri neskladju gremo levo do začetka *)
  |> add_transition "brisi_levo" '0' "brisi_levo" ' ' Left
  |> add_transition "brisi_levo" '1' "brisi_levo" ' ' Left
  |> add_transition "brisi_levo" ' ' "zapis_0" ' ' Right

    (* zapis_1: napišemo 1 na prvo celico in koncamo *)
  |> add_transition "zapis_1" '0' "koncaj" '1' Right
  |> add_transition "zapis_1" '1' "koncaj" '1' Right
  |> add_transition "zapis_1" ' ' "koncaj" '1' Right

    (* zapis_0: napišemo 0 na prvo celico in koncamo *)
  |> add_transition "zapis_0" '0' "koncaj" '0' Right
  |> add_transition "zapis_0" '1' "koncaj" '0' Right
  |> add_transition "zapis_0" ' ' "koncaj" '0' Right


(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na vhod sprejme niz `n` enic in na koncu na
  traku zapiše `n^2` enic.
[*----------------------------------------------------------------------------*)

let kvadrat_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme število `n`,
  zapisano v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih
  natanko `n` enic.
[*----------------------------------------------------------------------------*)

let enice_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
  sprejme število `n` enic, na koncu pa naj bo na traku zapisano število `n`
  v dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let dvojski_stroj : MachineUcinkovito.t = assert false

(*----------------------------------------------------------------------------*
  Sestavite Turingov stroj, ki na začetku na traku sprejme oklepaje `(` in
  `)`, `[` in `]` ter `{` in `}`. Stroj naj na traku izpiše `1`, če so
  oklepaji pravilno uravnoteženi in gnezdeni, ter `0` sicer.
[*----------------------------------------------------------------------------*)

let uravnotezeni_oklepaji_stroj : MachineUcinkovito.t = 
  let open MachineUcinkovito in
  make "start" []
  |> add_transition "start" '(' "start" '(' Right
  |> add_transition "start" '[' "start" '[' Right
  |> add_transition "start" '{' "start" '{' Right
  |> add_transition "start" 'x' "start" 'x' Right
  |> add_transition "start" ')' "zapomni(" ')' Left
  |> add_transition "start" ']' "zapomni[" ']' Left
  |> add_transition "start" '}' "zapomni{" '}' Left
  |> add_transition "start" ' ' "preveri" ' ' Left
    
  |> add_transition "zapomni(" 'x' "zapomni(" 'x' Left 
  |> add_transition "zapomni(" '(' "popravi(" 'x' Right
  |> add_transition "zapomni(" '[' "brisanje" '[' Right
  |> add_transition "zapomni(" '{' "brisanje" '{' Right
  |> add_transition "zapomni(" ']' "brisanje" ']' Right
  |> add_transition "zapomni(" '}' "brisanje" '}' Right
  |> add_transition "zapomni(" ')' "brisanje" ')' Right
    
  |> add_transition "zapomni[" 'x' "zapomni[" 'x' Left 
  |> add_transition "zapomni[" '[' "popravi[" 'x' Right
  |> add_transition "zapomni[" '(' "brisanje" '(' Right
  |> add_transition "zapomni[" '{' "brisanje" '{' Right
  |> add_transition "zapomni[" ']' "brisanje" ']' Right
  |> add_transition "zapomni[" '}' "brisanje" '}' Right
  |> add_transition "zapomni[" ')' "brisanje" ')' Right
    
  |> add_transition "zapomni{" 'x' "zapomni{" 'x' Left 
  |> add_transition "zapomni{" '{' "popravi{" 'x' Right
  |> add_transition "zapomni{" '(' "brisanje" '(' Right
  |> add_transition "zapomni{" '[' "brisanje" '[' Right
  |> add_transition "zapomni{" ']' "brisanje" ']' Right
  |> add_transition "zapomni{" '}' "brisanje" '}' Right
  |> add_transition "zapomni{" ')' "brisanje" ')' Right
    
  |> add_transition "popravi(" 'x' "popravi(" 'x' Right
  |> add_transition "popravi(" ')' "start" 'x' Right
  |> add_transition "popravi[" 'x' "popravi[" 'x' Right
  |> add_transition "popravi[" ']' "start" 'x' Right
  |> add_transition "popravi{" 'x' "popravi{" 'x' Right
  |> add_transition "popravi{" '}' "start" 'x' Right
    
  |> add_transition "preveri" ' ' "konec" '1' Right
  |> add_transition "preveri" 'x' "preveri" ' ' Left
  |> add_transition "preveri" '(' "brisanje" '(' Right
  |> add_transition "preveri" '[' "brisanje" '[' Right
  |> add_transition "preveri" '{' "brisanje" '{' Right
    
  |> add_transition "brisanje" 'x' "brisanje" 'x' Right
  |> add_transition "brisanje" '(' "brisanje" '(' Right
  |> add_transition "brisanje" ')' "brisanje" ')' Right
  |> add_transition "brisanje" '[' "brisanje" '[' Right
  |> add_transition "brisanje" ']' "brisanje" ']' Right
  |> add_transition "brisanje" '{' "brisanje" '{' Right
  |> add_transition "brisanje" '}' "brisanje" '}' Right
  |> add_transition "brisanje" ' ' "brisi" ' ' Left
    
  |> add_transition "brisi" 'x' "brisi" 'x' Left
  |> add_transition "brisi" '(' "brisi" ' ' Left
  |> add_transition "brisi" ')' "brisi" ' ' Left
  |> add_transition "brisi" '[' "brisi" ' ' Left
  |> add_transition "brisi" ']' "brisi" ' ' Left
  |> add_transition "brisi" '{' "brisi" ' ' Left
  |> add_transition "brisi" '}' "brisi" ' ' Left
  |> add_transition "brisi" ' ' "konec" '0' Right
