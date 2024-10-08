/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module std/data/sort

import std/core/undiv

pub fun sort(l: list<a>, ?cmp: (a, a) -> e order): e list<a>
  match l
    Nil -> Nil
    Cons(h, Nil) -> Cons(h, Nil)
    Cons(pivot, rst) -> 
      val (smaller, greater) = rst.partition fn(it) { cmp(it, pivot) == Lt }
      return sort(smaller.pretend-decreasing) ++ Cons(pivot, sort(greater.pretend-decreasing))

pub fun group(l: list<a>, ?(==): (a, a) -> e bool): e list<list<a>>
  match l
    Nil -> Nil
    Cons(h, Nil) -> Cons(Cons(h, Nil), Nil)
    Cons(pivot, rst) -> 
      val (smaller, greater) = rst.partition fn(it) { it == pivot }
      return group(smaller.pretend-decreasing) ++ Cons(Cons(pivot, greater), Nil)