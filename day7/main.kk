import std/os/path
import std/os/file

effect computer
    fun cd(where : string) : ()
    fun count-file(size : int) : ()
    
fun handle-cmd(line : list<string>) : <console,exn,computer> ()
    match line
        Cons("cd", args) -> cd(args[0].unjust)
        _ -> ()
    
fun handle-line(line : string) : <console,exn,computer> ()
    match line.split(" ")
        Cons("$", rest) -> handle-cmd(rest)
        Cons("dir", _) -> ()
        Cons(size, _) -> size.parse-int.unjust.count-file

fun find-index-internal(list : list<a>, pred : (a) -> e bool, idx : int) : e int
    match list
        Cons(x, xs) -> 
            if pred(x) then
                idx
            else
                find-index-internal(xs, pred, idx + 1)
        Nil -> -1

fun find-index(list : list<a>, pred : (a) -> e bool) : e int
    list.find-index-internal(pred, 0)
    
fun main() : <console,exn,div,fsys> ()
    var current-path := "/".path
    var current-path-idx := 0
    var paths : list<(path, int)> := Cons((current-path, 0), Nil)
    with handler
        fun cd(where)
            current-path := current-path / where
            val idx = paths.find-index fn((p, _))
                p.show == current-path.show
            if idx == -1 then
                current-path-idx := 0
                paths := Cons((current-path, 0), paths)
            else
                current-path-idx := idx
        fun count-file(size)
            val (rest, Cons((_, curr), xrest)) = paths.split(current-path-idx)
            paths := rest ++ Cons((current-path, curr + size), xrest)
    "assets/day7"
        .path
        .read-text-file
        .trim
        .lines
        .foreach(handle-line)
    val path-sizes-children = paths
        .map fn((p, s))
            val curr = p.string
            val children = paths.filter fn((po, _))
                val other = po.string
                curr != other && starts-with(other, curr).is-just
            val children-size = children
                .map( fn((_, sz)) sz )
                .foldl(0, (+))
            children-size + s
    val total-occupied = paths
        .map( fn((_, s)) s )
        .foldl1((+))
    val free-space = 70000000 - total-occupied
    val required = 30000000 - free-space

    val part1 = path-sizes-children.filter( fn(s) s <= 100000 )
        .foldl1((+))

    val part2 = path-sizes-children.filter( fn(s) s >= required )
        .minimum

    print("Part 1: "); println(part1)
    print("Part 2: "); println(part2)
