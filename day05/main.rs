use std::collections::VecDeque;

fn main() {
    let p = std::env::args().nth(2);
    let p = p.as_deref().unwrap_or("assets/day5");
    let f = std::fs::read_to_string(p).unwrap();
    let f = f.trim_end();

    let mut crates: Vec<VecDeque<u8>> = Vec::new();
    let mut moves = Vec::new();

    for line in f.lines() {
        if line.trim().starts_with("[") {
            for (i, c) in line.as_bytes().chunks(4).enumerate() {
                if c[1] != b' ' {
                    if crates.len() <= i {
                        crates.resize_with(i + 1, Default::default);
                    }
                    crates[i].push_front(c[1]);
                }
            }
        } else if line.starts_with("move") {
            let mut l = line.split(" ");
            let _ = l.next();
            let cnt: usize = l.next().unwrap().parse().unwrap();
            let _ = l.next();
            let from: usize = l.next().unwrap().parse().unwrap();
            let _ = l.next();
            let to: usize = l.next().unwrap().parse().unwrap();
            moves.push((cnt, from, to));
        }
    }

    let part1: String = {
        let mut crates = crates.clone();
        for (cnt, from, to) in moves.iter().copied() {
            let (from, to) = if from > to {
                let (a, b) = crates.split_at_mut(from - 1);
                (&mut b[0], &mut a[to - 1])
            } else {
                let (a, b) = crates.split_at_mut(to - 1);
                (&mut a[from - 1], &mut b[0])
            };
            for _ in 0..cnt {
                to.push_back(from.pop_back().unwrap());
            }
        }
        crates
            .into_iter()
            .map(|mut x| x.pop_back().unwrap() as char)
            .collect()
    };

    let part2: String = {
        for (cnt, from, to) in moves {
            let (from, to) = if from > to {
                let (a, b) = crates.split_at_mut(from - 1);
                (&mut b[0], &mut a[to - 1])
            } else {
                let (a, b) = crates.split_at_mut(to - 1);
                (&mut a[from - 1], &mut b[0])
            };
            let idx = from.len() - cnt;
            to.extend(from.make_contiguous()[idx..].iter().copied());
            from.truncate(idx);
        }
        crates
            .into_iter()
            .map(|mut x| x.pop_back().unwrap() as char)
            .collect()
    };

    println!("Part 1: {part1}",);
    println!("Part 2: {part2}",);
}
