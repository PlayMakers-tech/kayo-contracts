let rec _sqrt (n:int) (s:int) : int = if s*s > n then s-1 else _sqrt n (s+1)
let sqrt (n: int) : int = _sqrt n 1

let random (s0: nat) (s1: nat) = 
  let s1 =  s1 lxor (s1 lsl 23n) in 
  let s1 =  s1 lxor (s1 lsr 17n) in 
  let s1 =  s1 lxor s0 in 
  let s1 =  s1 lxor (s0 lsr 26n) in 
  s0 + s1

// Converts a supposedly random byte into a random nat (0-255)
let byte_to_nat (b: bytes) : nat =
  let i : int = Option.unopt(Bytes.unpack (Bytes.concat 0x0500 (
    if b < 0x80 then b else (Bytes.concat b 0x01) )))
  in abs (i+128)

[@inline]
let rand_hash () : bytes = 
  let f = 
  [%Michelson
    ({|{
        PUSH mutez 0; NONE key_hash;
        CREATE_CONTRACT {parameter unit; storage unit; code {CDR; NIL operation; PAIR}};
        DROP
    }|}
    : unit -> address)
  ]
  in Bytes.sub 10n 17n (Bytes.pack (f ()))

let rand_hash_as_nat () : nat = 
  let r =  rand_hash () in
  (byte_to_nat (Bytes.sub 0n 1n r)) + 256n * (
    (byte_to_nat (Bytes.sub 2n 1n r)) + 256n * (
     (byte_to_nat (Bytes.sub 3n 1n r)) + 256n * (
      (byte_to_nat (Bytes.sub 4n 1n r)) + 256n * (
       (byte_to_nat (Bytes.sub 5n 1n r)) + 256n * (
        (byte_to_nat (Bytes.sub 6n 1n r)) + 256n * (
         (byte_to_nat (Bytes.sub 7n 1n r)) + 256n * (
          (byte_to_nat (Bytes.sub 9n 1n r)) + 256n * (
           (byte_to_nat (Bytes.sub 10n 1n r)) + 256n * (
            (byte_to_nat (Bytes.sub 11n 1n r)) + 256n * (
             (byte_to_nat (Bytes.sub 12n 1n r)) + 256n * (
              (byte_to_nat (Bytes.sub 13n 1n r)) + 256n * (
               (byte_to_nat (Bytes.sub 14n 1n r)) + 256n * (
                (byte_to_nat (Bytes.sub 15n 1n r)) + 256n * (
                 (byte_to_nat (Bytes.sub 16n 1n r))))))))))))))))
  