module DataStructs
open System

[<RequireQualifiedAccess>]
module CustomSet =
    [<Struct>]
    type Entry<'a> =
        private
            {   HashCode    : int32 // Lower 31 bits of hash code, -1 if unused
                Next        : int32 // Index of next entry, -1 if last
                Value       : 'a
            }

    type T<'a> =
        private
            {   Buckets     : int32[] option
                Entries     : Entry<'a>[]
                Count       : int32
                FreeList    : int32
                FreeCount   : int32
            }

    let size (s : T<'a>) = s.Count - s.FreeCount

    let empty =
        {   Buckets     = None
            Entries     = Array.empty
            Count       = 0
            FreeList    = 0
            FreeCount   = 0
        }

    let rec private tryFindEntry (el : 'a) (set : T<'a>) =
        match set.Buckets with
        | None ->
            None

        | Some buckets ->
            let hashcode = (hash el) &&& 0x7FFFFFFF

            let rec loop idx =
                let entry = set.Entries.[idx]

                if (entry.HashCode = hashcode) then
                    Some idx
                else
                    match entry.Next with
                    | -1        -> None
                    | nextIdx   -> loop nextIdx

            in loop (buckets.[hashcode % buckets.Length])

    let add (el : 'a) (set : T<'a>) =
        match tryFindEntry el set with
        | Some idx ->
            { set with Entries = Array.zeroCreate 0 }
        
            
        
        // for (int i = buckets[targetBucket]; i >= 0; i = entries[i].next) {
        //         if (entries[i].hashCode == hashCode && comparer.Equals(entries[i].key, key)) {
        //             if (add) { 
        //                 ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_AddingDuplicate);
        //             }
        //             entries[i].value = value;
        //             version++;
        //             return;
        //         }

    let contains (el : 'a) (s : T<'a>) = tryFindEntry el s |> Option.isSome
