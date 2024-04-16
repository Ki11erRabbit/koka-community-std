/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
// This module provides a bucket hashmap 
module std/data/hash/bucket-hashmap
import std/core-extras

pub value struct bucket-hash-map<k, v>
  data : vector<maybe<list<(k,v)>>>
  amount : int

// Creates a bucket-hash-map with a specified inital `capacity`.
// If zero then the bucket-hash-map will set capacity to 1.
pub fun bucket-hash-map( capacity : int ) : bucket-hash-map<k, v>
  Bucket-hash-map( vector( if capacity > 0 then capacity else 1, Nothing ), 0 )

// Creates a bucket-hash-map from a list `xs`. This bucket-hash-map has the same capacity as the length of the list `xs`.
pub fun bucket-hash-map-from-list( xs : list<(k, v)>, ?hash : (k) -> int, ?(==) : (k, k) -> bool ) : <div, exn> bucket-hash-map<k, v>
  var bucket-hash-map := bucket-hash-map( xs.length )
  xs.foreach fn (x)
    val (key, value) = x
    bucket-hash-map := bucket-hash-map.insert(key, value)
  bucket-hash-map

// Resizes a bucket-hash-map with `new-capacity` being the new capacity of the hashmap. 
// If `new-capacity` is smaller than the capacity of the bucket-hash-map then there will be more hash collisions.
pub fun resize( hm : bucket-hash-map<k, v>, new-capacity : int, ?hash : (k) -> int ) : <div, exn> bucket-hash-map<k, v>
  val new-data = vector( new-capacity, Nothing ).rehash( hm )
  hm( data = new-data )

// Rehashes a bucket-hash-map's hash-table which is a vector<maybe<list<(k, v)>>>. This should not be called directly
fun rehash( v : vector<maybe<list<(k,v)>>>, old-map : bucket-hash-map<k, v>, ?hash : (k) -> int ) : <div, exn|e> vector<maybe<list<(k,v)>>>
  var vec := v
  val size = v.length
  old-map.data.vector/foreach() fn (bucket)
    match bucket
      Nothing -> ()
      Just(list) -> list.list/foreach() fn (item)
        val (key, _) = item
        val the-hash = hash(key)
        val position = the-hash % size
        match vec.at(position)
          Nothing -> ()// This should be impossible
          Just(Just(lst)) -> vec := vec.set( position, Just( Cons( item, lst ) ) ).unjust 
          Just(Nothing) -> vec := vec.set( position, Just( Cons( item, Nil ) ) ).unjust 
  vec

// Internal function that dictates when to resize the bucket-hash-map. This can be overridden with implicits.
fun when-resize( table-size : int, item-count : int ) : bool
  if 2 * table-size <= item-count then
    True
  else
    False

// Internal function that determines the new size of a bucket-hash-map when inserting. This can be overridden with implicits.
fun resizer( table-size : int, item-count : int ) : int
   item-count * 2

// Inserts a `value` into the bucket-hash-map with a given `key`.
pub fun insert( hm : bucket-hash-map<k,v>, key : k, value : v, ?hash : (k) -> int, ?(==) : (k, k) -> bool, ?when-resize : (int, int) -> bool, ?resizer : (int, int) -> int ) : <div, exn> bucket-hash-map<k, v>
  val the-hash = hash(key)
  val position = the-hash % hm.data.length
  match hm.data.at(position)
    Nothing -> hm // This should be impossible
    Just(Just(list)) -> hm( data = hm.data.set( position, Just( Cons( (key, value), list.filter() fn(x) not(x.fst == key) ) ) ).unjust, amount = hm.amount + 1 )
    Just(Nothing) -> hm( data = hm.data.set( position, Just( Cons( (key, value), Nil ) ) ).unjust, amount = hm.amount + 1 )
  if when-resize( hm.data.length, hm.amount ) then
    hm.resize( resizer( hm.data.length, hm.amount ) )
  else
    hm

// Removes a value from the bucket-hash-map with a given `key`.
pub fun remove( hm : bucket-hash-map<k,v>, key : k, ?hash : (k) -> int, ?(==) : (k, k) -> bool ) : exn bucket-hash-map<k, v>
  val the-hash = hash(key)
  val position = the-hash % hm.data.length
  match hm.data.at(position)
    Nothing -> hm // This should be impossible
    Just(Just(list)) -> hm( data = hm.data.set( position, Just( list.filter() fn(x) not(x.fst == key) ) ).unjust, amount = hm.amount - 1 )
    Just(Nothing) -> hm // quietly fail

// Attempts to retreve a value from the bucket-hash-map with a given `key`. Returns `Nothing` if the key is invalid.
pub fun get( hm : bucket-hash-map<k, v>, key : k, ?hash : (k) -> int, ?(==) : (k, k) -> bool ) : maybe<v>
  val the-hash = hash(key)
  val position = the-hash % hm.data.length
  match hm.data.at(position)
    Nothing -> Nothing // This should be impossible
    Just(Just(list)) -> match (list.filter() fn(x) x.fst == key)
      Cons(x, _) -> Just(x.snd)
      Nil -> Nothing
    Just(Nothing) -> Nothing

// Fetches the amount of items in the bucket-hash-map
pub fun length( hm : bucket-hash-map<k, v> ) : int
  hm.amount

// Removes all buckets from the bucket-hash-map, effectively clearing the bucket-hash-map.
pub fun clear( hm : bucket-hash-map<k, v>) : bucket-hash-map<k, v>
  for( hm.data.length ) fn (x) 
    hm.data.unsafe-assign(x.ssize_t, Nothing)
  hm

// Applies a function `f` to each value of the bucket-hash-map. This exposes the key to `f`.
pub fun map( hm : bucket-hash-map<k, a>, f : (k, a) -> b ) : bucket-hash-map<k, b>
  var new-hm := bucket-hash-map( hm.data.length )
  val new-vec = hm.data.vector/map() fn (bucket)
    match bucket
      Nothing -> Nothing
      Just(list) -> 
        val new-list = list.list/map() fn (item) 
          val (key, value) = item
          (key, f( key, value ))
        Just(new-list)   
  Bucket-hash-map( new-vec, hm.amount )

// Invoke a function `f` for each value of the bucket-hash-map.
pub fun foreach( hm : bucket-hash-map<k, v>, f : (k, v) -> e () ) : e ()
  hm.data.foreach() fn (bucket)
    match bucket 
      Nothing -> ()
      Just(bkt) -> bkt.foreach() fn (item)
        val (key, value) = item
        f(key, value)