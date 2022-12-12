fn all_different<T: Eq>(slice: &[T]) -> bool {
    for i in 1..slice.len() {
        if slice[..i].iter().any(|x| *x == slice[i]) {
            return false;
        }
    }
    true
}

fn main() {
    let p = std::env::args().nth(1);
    let p = p.as_deref().unwrap_or("assets/day6");
    let f = std::fs::read_to_string(p).unwrap();
    let f = f.trim();

    let part1 = f.as_bytes().windows(4).position(all_different).unwrap() + 4;
    println!("Part 1: {part1}");

    let part2 = f.as_bytes().windows(14).position(all_different).unwrap() + 14;
    println!("Part 2: {part2}");
}
