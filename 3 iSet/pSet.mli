(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Polymorphic Set.

        This is a polymorphic set, similar to standard library [Set] module
        but in a defunctorized style.
*)

type 'a t

val empty : 'a t
(** The empty set, using [Pervasives.compare] as key comparison function. *)

val create : ('a -> 'a -> int) -> 'a t
(** creates a new empty set, using the provided function for key comparison.*)

val is_empty : 'a t -> bool
(** returns true if the set is empty. *)

val add : 'a -> 'a t -> 'a t
(** [add x s] returns a set containing the same elements as [s], plus
    [x]. If an element [y] equal to [x] was already in [s], the
    resulting set contains [x] and not [y]. *)

val remove : 'a -> 'a t -> 'a t
(** [remove x s] returns a set containing the same elements as [s],
    except for [x] which is missing in the returned set. *)

val mem : 'a -> 'a t -> bool
(** [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] to all elements in set [s].  The elements
    are passed to [f] in increasing order with respect to the ordering
    used to create the set. *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are the elements of s, in increasing order. *)

val elements : 'a t -> 'a list
(** Return the list of all elements of the given set.
    The returned list is sorted in increasing order with respect
    to the ordering used to create the set. *)

val split : 'a -> 'a t -> 'a t * bool * 'a t
(** [split x s] returns a triple [(l, present, r)], where
    [l] is the set of elements of [s] that are strictly less than [x];
    [r] is the set of elements of [s] that are strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x],
    or [true] if [s] contains an element equal to [x]. *)
