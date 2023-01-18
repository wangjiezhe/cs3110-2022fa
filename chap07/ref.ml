type 'a ref = { mutable contents : 'a }

let ( ! ) r = r.contents
let ( := ) r x = r.contents <- x
