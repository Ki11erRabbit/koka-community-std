/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module std/data/linearmap

pub alias linearMap<k,v> = list<(k,v)> 

pub fun show(l: linearMap<k,v>, ?key/show: (k) -> e string, ?value/show: (v) -> e string): e string
  "{" ++ l.map(fn((k,v)) key/show(k) ++ ": " ++ value/show(v)).join(",") ++ "}"

pub fun (==)(l1: linearMap<k,v>, l2: linearMap<k,v>, ?key/(==): (k, k) -> e bool, ?value/(==): (v, v) -> e bool): e bool
  l1.length == l2.length && l1.all fn(x) 
    match l2.lookup(x.fst)
      Just(v') -> v' == x.snd
      Nothing -> False

pub fun key/contains(l: list<(k,v)>, k: k, ?(==): (k, k) -> e bool): e bool
  l.any(fn((kk, _)) kk == k)

pub fun update(l: list<(k,v)>, k: k, b: v, f: (v, v) -> e v, ?(==): (k, k) -> e bool): e list<(k,v)>
  match l
    Cons((kk, vv), rst) -> if k == kk then Cons((kk, f(b, vv)), rst) else Cons((kk, vv), rst.update(k, b, f))
    Nil -> Cons((k, b), Nil) 

pub fun get(l: list<(k,v)>, k: k, ?(==): (k, k) -> e bool): <exn|e> v
  match l
    Cons((kk, vv), rst) -> if mask<exn>{kk == k} then vv else rst.get(k)
    Nil -> throw("not found")

pub fun map/lookup(m: linearMap<k,v>, k: k, ?(==): (k, k) -> e bool): e maybe<v>
  catch({Just(m.get(k))}, fn(x) {Nothing})

pub fun get-default(l: list<(k,v)>, k: k, v: v, ?(==): (k, k) -> e bool): e v
  match l
    Cons((kk, vv), rst) -> if kk == k then vv else rst.get-default(k, v)
    Nil -> v

/// Overwrite any existing key with value
pub fun set(m: linearMap<k,v>, k: k, v: v, ?(==): (k, k) -> e bool): e linearMap<k,v>
  m.update(k, v, fn(_, v1) v1)

/// Add only if key is not present in the map
pub fun add(m: linearMap<k,v>, k: k, b: v, ?(==): (k, k) -> e bool): e linearMap<k,v>
  m.update(k, b, fn(x, _) x)

pub fun add-all(m: linearMap<k,v>, l: list<(k,v)>, ?(==): (k, k) -> e bool): e linearMap<k,v>
  match l
    Nil -> m
    Cons((k, v), rst) -> add-all(m.add(k, v), rst)

pub fun foldr(m: linearMap<k,v>, acc: a, f: (k, v, a) -> e a): e a
  m.foldr(acc, fn((k, v), x) f(k, v, x))

pub fun map(m: linearMap<k,v>, f: (k, v) -> e (k, v)): e linearMap<k,v>
  m.map(fn((k,v)) f(k,v))

pub fun unions(l: list<linearMap<k,v>>, ?(==): (k, k) -> <exn|e> bool): <exn|e> linearMap<k,v>
  val fst = l.head.default([])
  l.foldl(fst, fn(x, y) x.add-all(y))

pub fun list-add/(+)(l1: linearMap<k,v>, l2: list<(k,v)>, ?(==): (k, k) -> e bool): e linearMap<k,v>
  l1.add-all(l2)

pub fun union(l1: linearMap<k,v>, l2: linearMap<k,v>, ?(==): (k, k) -> e bool): e linearMap<k,v>
  l1.add-all(l2)

pub fun filter(m: linearMap<k,v>, f: (k, v) -> e bool): e linearMap<k,v>
  m.filter(fn((k,v)) f(k,v))

pub fun remove(m: linearMap<k,v>, k: k, ?(==): (k, k) -> e bool): e linearMap<k,v>
  m.filter(fn((k1,_)) !(k == k1))

pub fun remove-all(m: linearMap<k,v>, l: list<k>, ?(==): (k, k) -> e bool): e linearMap<k,v>
  m.filter(fn((k1,_)) !l.any(fn(k2) k1 == k2))

pub fun union-with(m1: linearMap<k,v>, m2: linearMap<k,v>, f: (v, v) -> e v, ?(==): (k, k) -> e bool): e linearMap<k,v>
  m2.foldl(m1, fn(x, (k, y)) x.update(k, y, f))