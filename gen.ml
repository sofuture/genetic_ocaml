open Printf;;

type gene = {fitness : int; dna : string};;
type pair = {a : int; b : int};;

(* goal that we're looking to evolve to *)
let goal = "Hello, World!";;
let glen = (String.length goal);;

(* mutate genes 1/freak_factor times *)
let freak_factor = 30;;

(* string utilities *)
let explode s =
    let rec f acc = function
        | -1 -> acc
        | k -> f (s.[k] :: acc) (k - 1)
    in f [] (String.length s - 1);;

let implode l =
    let s = String.create (List.length l) in
    let rec f n = function
        | x :: xs -> s.[n] <- x; f (n + 1) xs
        | [] -> s
    in f 0 l;;

(* get a random printable char *)
let rand_pchar () =
    let n = 33 + (Random.int (127-33)) in
    Char.chr n;;

(* comparator for genes *)
let cmp_genes a b =
    compare a.fitness b.fitness;;

(* sort a list of genes *)
let sort_gp l = 
    List.sort (cmp_genes) l;;

(* compute the fitness of a gene *)
let fitness dna =
    let rec inner g s acc =
        match s with 
        | [] -> acc
        | h :: t ->
            let gd = Char.code (List.hd g) in
            let gt = List.tl g in
            let d = Char.code h in
            let diff = gd - d in
            inner gt t (acc + (diff*diff) ) in 
    inner (explode goal) (explode dna) 0;;

(* make a random gene *)
let rand_gene () =
    let rec inner c acc =
        if c == 0 then
            acc
        else
            inner (c-1) (List.rev_append acc [rand_pchar ()]) in
    let dn = implode (inner glen []) in
    let fit = fitness dn in
    {fitness = fit; dna = dn};;

(* seed genepool with random genes *)
let rec seed gp gens =
    if gens == 0 then
        gp
    else
        seed (List.rev_append gp [ rand_gene () ]) (gens - 1);;

(* print a list of genes *)
let rec disp_gp gp =
    match gp with
    | [] ->
        printf "=================\n"
    | h :: t ->
        printf "%d:%s\n" h.fitness h.dna;
        disp_gp t;;

(* pick a random parent *)
let random_mom len =
    Random.int len;;

(* pick another random parent *)
let rec random_dad len mom =
    let a = Random.int len in
    if a == mom then
        random_dad len mom
    else
        a;;

let make_baby mom dad =
    let md = explode mom.dna in
    let dd = explode dad.dna in
    let rec inner m d acc =
        match m with
        | [] -> acc
        | mh :: mt ->
            let dh = List.hd d in
            let dt = List.tl d in
            if (Random.int freak_factor) == 0 then
                (* mutation *)
                inner mt dt ([rand_pchar ()] @ acc)
            else if Random.bool () then
                (* maternal reproduction *)
                inner mt dt ([mh] @ acc)
            else
                (* paternal reproduction *)
                inner mt dt ([dh] @ acc) in
    let bdna_exploded = inner md dd [] in
    let bdna = implode bdna_exploded in
    if freak_factor = 0 then
        rand_gene ()
    else
        {fitness = (fitness bdna); dna = bdna};;

(* do the damn thing *)
let procreate gp cnt =
    let gp_size = List.length gp in
    let rec inner gp cnt = 
        if cnt == 0 then
            gp
        else
            (* select 2 parents *)
            let momi = random_mom gp_size in
            let dadi = random_dad gp_size momi in
            let mom = List.nth gp momi in
            let dad = List.nth gp dadi in
            let baby = make_baby mom dad in
            (* put baby in gp *)
            let spop = List.sort (cmp_genes) ([baby] @ gp) in
            (* drop least fit gp member *)
            let ngp = List.tl (List.rev spop) in
            inner ngp (cnt - 1) in
    inner gp cnt;;

(* main *)
let () =
    Random.self_init ();
    if (Array.length Sys.argv) == 1 then
        printf "usage: %s <gene pool size> <generations>\n" Sys.argv.(0)
    else
        let gp_size = int_of_string(Sys.argv.(1)) in
        let generations = int_of_string(Sys.argv.(2)) in
        let gp = sort_gp (seed [] gp_size) in
        disp_gp gp;
        disp_gp (procreate gp generations);;
