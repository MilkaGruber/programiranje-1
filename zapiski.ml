(* FUNKCIJSKI TIPI *)

(*tipe definiramo na sledeč način*)

(*običajno tipe vsilimo na argumente, lahko pa tudi na desni*)
let konjugiraj ((x, y): (float * float)) = ((x, -. y) : (float * float))

type kompleksno= float * float
let konjugiraj' ((x, y): kompleksno) : kompleksno = (x, -. y)

type slovar_kljuci_stringi_vrednosti_inti = (string * int) list
type ('k, 'v) slovar = ('k * 'v) list

type predmet = string
type ocena = int
type student = {
  ime: string;
  priimek: string;
  vpisna: int;
  ocene: (predmet, ocena) slovar
}

let matija = {
  ime = "Matija";
  priimek = "Pretnar";
  vpisna = 27003365;
  ocene = []
}
 
let opravi_izpit student predmet ocena = 
  {student with ocene = (predmet, ocena) :: student.ocene}

let matija_danes = 
  matija_vcasih
  