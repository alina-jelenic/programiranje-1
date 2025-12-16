# Turingovi stroji

## Naloga 1

**Definirajte k-tračni turingov stroj**

definicija: Turingov stroj je peterica (\Gamma, prazen, množica stanj, začetno stanje, prehodna funkcija)
prehodna funkcija: stanje x simbol -> stanje x simbol x {L, R}
Prazen je elemnet abecede in zacetno stanje je element stanj.

Enako kot eno-tračni turingov stroj, le da je tranzicijska funkcija oblike
$ Q \times \Gamma^k \to Q \times \Gamma^k \times \{L, R\}^k$

## Naloga 2

**Dokažite, da lahko enotračni turingov stroj, kjer prehodna funkcija omogoča, da se glava ostane na mestu, simulira enotračni turingov stroj, kjer se glava vedno premakne levo ali desno.**


## Naloga 3

Dan je jezik $J = \{ xyz | x, y, z \in \{0, 1\}^*, |x| = |y| = |z|, x (xor) y = z \}$. a = prazen

1. Sestavite deterministični n-tračni turingov stroj, ki preveri, ali je dana beseda v jeziku $J$.

razbijemo na 3 trakove, potem pa gledamo xor na te treh.

2. Sestavite deterministični enotračni turingov stroj, ki preveri, ali je dana beseda v jeziku $J$.

## Naloga 4

Pokažite, da lahko vsak n-tračni turingov stroj simuliramo z enotračnim turingovim strojem.

