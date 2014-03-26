(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

module type SeededS = sig
  include Hashtbl.SeededS
  val stats_alive: 'a t -> Hashtbl.statistics
    (** same as {!stats} but only count the alive bindings *)
end

module type S = sig
  include Hashtbl.S
  val stats_alive: 'a t -> Hashtbl.statistics
    (** same as {!stats} but only count the alive bindings *)
end

module StdObj = Obj

module Obj = struct

  type t

  external create: int -> t = "caml_ephe_create"

  let length x = Obj.size(Obj.repr x) - 2

  external get_key: t -> int -> Obj.t option = "caml_ephe_get_key"
  external get_key_copy: t -> int -> Obj.t option = "caml_ephe_get_key_copy"
  external set_key: t -> int -> Obj.t -> unit = "caml_ephe_set_key"
  external unset_key: t -> int -> unit = "caml_ephe_unset_key"
  external check_key: t -> int -> bool = "caml_ephe_check_key"
  external blit_key : t -> int -> t -> int -> int -> unit = "caml_ephe_blit_key"

  external get_data: t -> Obj.t option = "caml_ephe_get_data"
  external get_data_copy: t -> Obj.t option = "caml_ephe_get_data_copy"
  external set_data: t -> Obj.t -> unit = "caml_ephe_set_data"
  external unset_data: t -> unit = "caml_ephe_unset_data"
  external check_data: t -> bool = "caml_ephe_check_data"
  external blit_data : t -> t -> unit = "caml_ephe_blit_data"

  type equal =
  | ETrue | EFalse
  | EDead (** the garbage collector reclaimed the data *)

  module MakeSeeded(H: sig
    type t
    type 'a container
    val create: t -> 'a -> 'a container
    val hash: int -> t -> int
    val equal: t -> 'a container -> equal
    val get_data: 'a container -> 'a option
    val get_key: 'a container -> t option
    val set_data: 'a container -> 'a -> unit
    val check_key: 'a container -> bool
  end) : SeededS with type key = H.t = struct

    type 'a t =
      { mutable size: int;                        (* number of entries *)
        mutable data: 'a bucketlist array;  (* the buckets *)
        mutable seed: int;                        (* for randomization *)
        initial_size: int;                        (* initial array size *)
      }

    and 'a bucketlist =
    | Empty
    | Cons of int (** hash of the key *) * 'a H.container * 'a bucketlist

    (** the hash of the key is kept in order to test the equality of the hash
      before the key. Same reason than for Weak.Make *)

    type key = H.t

    let rec power_2_above x n =
      if x >= n then x
      else if x * 2 > Sys.max_array_length then x
      else power_2_above (x * 2) n

    let prng = lazy (Random.State.make_self_init())

    let create ?(random = (Hashtbl.is_randomized ())) initial_size =
      let s = power_2_above 16 initial_size in
      let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
      { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

    let clear h =
      h.size <- 0;
      let len = Array.length h.data in
      for i = 0 to len - 1 do
        h.data.(i) <- Empty
      done

    let reset h =
      let len = Array.length h.data in
      if len = h.initial_size then
        clear h
      else begin
        h.size <- 0;
        h.data <- Array.make h.initial_size Empty
      end

    let copy h = { h with data = Array.copy h.data }

    let key_index h hkey =
      hkey land (Array.length h.data - 1)

    let resize indexfun h =
      let odata = h.data in
      let osize = Array.length odata in
      let nsize = osize * 2 in
      if nsize < Sys.max_array_length then begin
        let ndata = Array.make nsize Empty in
        h.data <- ndata;        (* so that indexfun sees the new bucket count *)
        let rec insert_bucket = function
            Empty -> ()
          | Cons(key, data, rest) ->
              insert_bucket rest; (* preserve original order of elements *)
              let nidx = indexfun h key in
              ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
        for i = 0 to osize - 1 do
          insert_bucket odata.(i)
        done
      end

    let add h key info =
      let hkey = H.hash h.seed key in
      let i = key_index h hkey in
      let container = H.create key info in
      let bucket = Cons(hkey, container, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let remove h key =
      let hkey = H.hash h.seed key in
      let rec remove_bucket = function
        | Empty -> Empty
        | Cons(hk, c, next) when hkey = hk ->
            begin match H.equal key c with
            | ETrue -> h.size <- h.size - 1; next
            | EFalse -> Cons(hk, c, remove_bucket next)
            | EDead -> remove_bucket next (** The key have been reclaimed *)
            end
        | Cons(hk,c,next) -> Cons(hk, c, remove_bucket next) in
      let i = key_index h hkey in
      h.data.(i) <- remove_bucket h.data.(i)

    let rec find_rec key hkey = function
      | Empty ->
          raise Not_found
      | Cons(hk, c, rest) when hkey = hk  ->
          begin match H.equal key c with
          | ETrue ->
              begin match H.get_data c with
              | None ->
                  (** This case is not impossible because the gc can run between
                      H.equal and H.get_data *)
                  (** TODO? remove this dead key *)
                  find_rec key hkey rest
              | Some d -> d
              end
          | EFalse -> find_rec key hkey rest
          | EDead ->
              (** TODO? remove this dead key *)
              find_rec key hkey rest
          end
      | Cons(_, _, rest) ->
          find_rec key hkey rest

    let find h key =
      let hkey = H.hash h.seed key in
      (** TODO inline 3 iteration *)
      find_rec key hkey (h.data.(key_index h hkey))

    let find_all h key =
      let hkey = H.hash h.seed key in
      let rec find_in_bucket = function
      | Empty -> []
      | Cons(hk, c, rest) when hkey = hk  ->
          begin match H.equal key c with
          | ETrue -> begin match H.get_data c with
              | None ->
                  (** TODO? remove this dead key *)
                  find_in_bucket rest
              | Some d -> d::find_in_bucket rest
            end
          | EFalse -> find_in_bucket rest
          | EDead ->
              (** TODO? remove this dead key *)
              find_in_bucket rest
          end
      | Cons(_, _, rest) ->
          find_in_bucket rest in
      find_in_bucket h.data.(key_index h hkey)


    let replace h key info =
      let hkey = H.hash h.seed key in
      let rec replace_bucket = function
        | Empty -> raise Not_found
        | Cons(hk, c, next) when hkey = hk ->
            begin match H.equal key c with
          | ETrue -> begin match H.get_data c with
              | None ->
                  (** Can this case really happend? *)
                  (** TODO? remove this dead key *)
                  replace_bucket next
              | Some d -> H.set_data c info
            end
          | EFalse -> replace_bucket next
          | EDead ->
              (** TODO? remove this dead key *)
              replace_bucket next
            end
        | Cons(_,_,next) -> replace_bucket next
      in
      let i = key_index h hkey in
      let l = h.data.(i) in
      try
        replace_bucket l
      with Not_found ->
        let container = H.create key info in
        h.data.(i) <- Cons(hkey, container, l);
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h

    let mem h key =
      let hkey = H.hash h.seed key in
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(hk, c, rest) when hk = hkey ->
          begin match H.equal key c with
          | ETrue -> true
          | EFalse -> mem_in_bucket rest
          | EDead ->
              (** TODO? remove this dead key *)
              mem_in_bucket rest
          end
      | Cons(hk, c, rest) -> mem_in_bucket rest in
      mem_in_bucket h.data.(key_index h hkey)

    let iter f h =
      let rec do_bucket = function
        | Empty ->
            ()
        | Cons(_, c, rest) ->
            begin match H.get_key c, H.get_data c with
            | None, _ | _, None -> (** TODO? remove this dead key? *) ()
            | Some k, Some d -> f k d
            end; do_bucket rest in
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        do_bucket d.(i)
      done

    let fold f h init =
      let rec do_bucket b accu =
        match b with
          Empty ->
            accu
        | Cons(_, c, rest) ->
            let accu = begin match H.get_key c, H.get_data c with
              | None, _ | _, None -> (** TODO? remove this dead key? *) accu
              | Some k, Some d -> f k d accu
            end in
            do_bucket rest accu  in
      let d = h.data in
      let accu = ref init in
      for i = 0 to Array.length d - 1 do
        accu := do_bucket d.(i) !accu
      done;
      !accu

    let length h = h.size


    let rec bucket_length accu = function
      | Empty -> accu
      | Cons(_, _, rest) -> bucket_length (accu + 1) rest

    let stats h =
      let mbl =
        Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
      let histo = Array.make (mbl + 1) 0 in
      Array.iter
        (fun b ->
           let l = bucket_length 0 b in
           histo.(l) <- histo.(l) + 1)
        h.data;
      { Hashtbl.num_bindings = h.size;
        num_buckets = Array.length h.data;
        max_bucket_length = mbl;
        bucket_histogram = histo }

    let rec bucket_length_alive accu = function
      | Empty -> accu
      | Cons(_, c, rest) when H.check_key c ->
          bucket_length_alive (accu + 1) rest
      | Cons(_, _, rest) -> bucket_length_alive accu rest

    let stats_alive h =
      let size = ref 0 in
      let mbl =
        Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
      let histo = Array.make (mbl + 1) 0 in
      Array.iter
        (fun b ->
           let l = bucket_length 0 b in
           size := !size + l;
           histo.(l) <- histo.(l) + 1)
        h.data;
      { Hashtbl.num_bindings = !size;
        num_buckets = Array.length h.data;
        max_bucket_length = mbl;
        bucket_histogram = histo }


  end

end

let obj_opt : StdObj.t option -> 'a option = fun x ->
  match x with
  | None -> x
  | Some v -> Some (StdObj.obj v)

(** The previous function is typed so this one is also correct *)
let obj_opt : StdObj.t option -> 'a option = fun x -> StdObj.magic x


module K1 = struct
  type ('k,'d) t = Obj.t

  let create () : ('k,'d) t = Obj.create 1

  let get_key (t:('k,'d) t) : 'k option = obj_opt (Obj.get_key t 0)
  let get_key_copy (t:('k,'d) t) : 'k option = obj_opt (Obj.get_key_copy t 0)
  let set_key (t:('k,'d) t) (k:'k) : unit = Obj.set_key t 0 (StdObj.repr k)
  let unset_key (t:('k,'d) t) : unit = Obj.unset_key t 0
  let check_key (t:('k,'d) t) : bool = Obj.check_key t 0

  let blit_key (t1:('k,'d) t) (t2:('k,'d) t) : unit = Obj.blit_key t1 0 t2 0 1

  let get_data (t:('k,'d) t) : 'd option = obj_opt (Obj.get_data t)
  let get_data_copy (t:('k,'d) t) : 'd option = obj_opt (Obj.get_data_copy t)
  let set_data (t:('k,'d) t) (d:'d) : unit = Obj.set_data t (StdObj.repr d)
  let unset_data (t:('k,'d) t) : unit = Obj.unset_data t
  let check_data (t:('k,'d) t) : bool = Obj.check_data t
  let blit_data (t1:(_,'d) t) (t2:(_,'d) t) : unit = Obj.blit_data t1 t2

  module MakeSeeded (H:Hashtbl.SeededHashedType) =
    Obj.MakeSeeded(struct
      type 'a container = (H.t,'a) t
      type t = H.t
      let create k d =
        let c = create () in
        set_data c d;
        set_key c k;
        c
      let hash = H.hash
      let equal k c =
        match get_key c with
        | None -> Obj.EDead
        | Some k' -> if H.equal k k' then Obj.ETrue else Obj.EFalse
      let get_data = get_data
      let get_key = get_key
      let set_data = set_data
      let check_key = check_key
    end)

  module Make(H: Hashtbl.HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
  end

end

module K2 = struct
  type ('k1, 'k2, 'd) t = Obj.t

  let create () : ('k1,'k2,'d) t = Obj.create 1

  let get_key1 (t:('k1,'k2,'d) t) : 'k1 option = obj_opt (Obj.get_key t 0)
  let get_key1_copy (t:('k1,'k2,'d) t) : 'k1 option =
    obj_opt (Obj.get_key_copy t 0)
  let set_key1 (t:('k1,'k2,'d) t) (k:'k1) : unit =
    Obj.set_key t 0 (StdObj.repr k)
  let unset_key1 (t:('k1,'k2,'d) t) : unit = Obj.unset_key t 0
  let check_key1 (t:('k1,'k2,'d) t) : bool = Obj.check_key t 0

  let get_key2 (t:('k1,'k2,'d) t) : 'k2 option = obj_opt (Obj.get_key t 1)
  let get_key2_copy (t:('k1,'k2,'d) t) : 'k2 option =
    obj_opt (Obj.get_key_copy t 1)
  let set_key2 (t:('k1,'k2,'d) t) (k:'k2) : unit =
    Obj.set_key t 1 (StdObj.repr k)
  let unset_key2 (t:('k1,'k2,'d) t) : unit = Obj.unset_key t 1
  let check_key2 (t:('k1,'k2,'d) t) : bool = Obj.check_key t 1


  let blit_key1 (t1:('k1,_,_) t) (t2:('k1,_,_) t) : unit =
    Obj.blit_key t1 0 t2 0 1
  let blit_key2 (t1:(_,'k2,_) t) (t2:(_,'k2,_) t) : unit =
    Obj.blit_key t1 1 t2 1 1
  let blit_key12 (t1:('k1,'k2,_) t) (t2:('k1,'k2,_) t) : unit =
    Obj.blit_key t1 0 t2 0 2

  let get_data (t:('k1,'k2,'d) t) : 'd option = obj_opt (Obj.get_data t)
  let get_data_copy (t:('k1,'k2,'d) t) : 'd option =
    obj_opt (Obj.get_data_copy t)
  let set_data (t:('k1,'k2,'d) t) (d:'d) : unit = Obj.set_data t (StdObj.repr d)
  let unset_data (t:('k1,'k2,'d) t) : unit = Obj.unset_data t
  let check_data (t:('k1,'k2,'d) t) : bool = Obj.check_data t
  let blit_data (t1:(_,_,'d) t) (t2:(_,_,'d) t) : unit = Obj.blit_data t1 t2

  module MakeSeeded
      (H1:Hashtbl.SeededHashedType)
      (H2:Hashtbl.SeededHashedType) =
    Obj.MakeSeeded(struct
      type 'a container = (H1.t,H2.t,'a) t
      type t = H1.t * H2.t
      let create (k1,k2) d =
        let c = create () in
        set_data c d;
        set_key1 c k1; set_key2 c k2;
        c
      let hash seed (k1,k2) =
        H1.hash seed k1 + H2.hash seed k2 * 65599
      let equal (k1,k2) c =
        match get_key1 c, get_key2 c with
        | None, _ | _ , None -> Obj.EDead
        | Some k1', Some k2' ->
            if H1.equal k1 k1' && H2.equal k2 k2'
            then Obj.ETrue else Obj.EFalse
      let get_data = get_data
      let get_key c =
        match get_key1 c, get_key2 c with
        | None, _ | _ , None -> None
        | Some k1', Some k2' -> Some (k1', k2')
      let set_data = set_data
      let check_key c = check_key1 c && check_key2 c
    end)

  module Make(H1: Hashtbl.HashedType)(H2: Hashtbl.HashedType):
    (S with type key = H1.t * H2.t) =
  struct
    include MakeSeeded
        (struct
          type t = H1.t
          let equal = H1.equal
          let hash (seed: int) x = H1.hash x
        end)
        (struct
          type t = H2.t
          let equal = H2.equal
          let hash (seed: int) x = H2.hash x
        end)
    let create sz = create ~random:false sz
  end

end
