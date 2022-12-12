import std/os/path
import std/os/file

fun identity(v : a) : a
    v

fun succ(v : int) : int
    v + 1

fun main() : <console,exn,fsys> ()
    val board = "assets/day8"
        .path
        .read-text-file
        .trim
        .lines
        .map fn(line)
            line
                .split("")
                .map fn(elem)
                    elem.parse-int.unjust

    val part1 = board
        .map-indexed fn(y, row)
            row.map-indexed fn(x, elem)
                val (left, Cons(_, right)) = row.split(x)
                val (up, Cons(_, down)) = board
                    .map( fn(column) column[x].unjust )
                    .split(y)
                up.all( fn(h) h < elem ) ||
                down.all( fn(h) h < elem ) || 
                left.all( fn(h) h < elem ) || 
                right.all( fn(h) h < elem )
        .flatmap(identity)
        .filter(identity)
        .length
        
    val part2 = board
        .map-indexed fn(y, row)
            row.map-indexed fn(x, elem)
                val (left, Cons(_, right)) = row.split(x)
                val (up, Cons(_, down)) = board
                    .map( fn(column) column[x].unjust )
                    .split(y)
                val left-vis = left.reverse
                    .take-while( fn(h) h < elem )
                    .length
                    .succ
                    .min(left.length)
                val up-vis = up.reverse
                    .take-while( fn(h) h < elem )
                    .length
                    .succ
                    .min(up.length)
                val right-vis = right
                    .take-while( fn(h) h < elem )
                    .length
                    .succ
                    .min(right.length)
                val down-vis = down
                    .take-while( fn(h) h < elem )
                    .length
                    .succ
                    .min(down.length)
                left-vis * right-vis * up-vis * down-vis
        .flatmap(identity)
        .maximum
    
    print("Part 1: "); println(part1)
    print("Part 2: "); println(part2)
