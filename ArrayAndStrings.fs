module ArrayAndStrings

open DataStructs

let areEachSymbolMeetsOnce (txt : string) =
    let rec loop (rest : char seq) (set: Set<char>) =
        let maybeCh = Seq.tryHead rest
        match maybeCh with
        | Some ch ->
            if set |> Set.contains ch then
                false
            else
                loop (rest |> Seq.skip 1) (set |> Set.add ch)

        | None ->
            true
    in
        loop txt Set.empty

let areEachSymbolMeetsOnceWithCustomHash (txt : string) =
    let rec loop (rest : char seq) (set: CustomSet.T<char>) =
        let maybeCh = Seq.tryHead rest
        match maybeCh with
        | Some ch ->
            if set |> CustomSet.contains ch then
                false
            else
                loop (rest |> Seq.skip 1) (set |> CustomSet.add ch)

        | None ->
            true
    in
        loop txt CustomSet.empty