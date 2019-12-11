use num::Integer;
use std::error::Error;
use std::f64::consts::PI;

fn print_grid(s: &Vec<Vec<u8>>) {
    for line in s.iter() {
        println!("{}", unsafe { String::from_utf8_unchecked(line.clone()) });
    }

    println!();
}

fn part1() -> Result<(), Box<dyn Error>> {
    let mut lines = vec![
        ".#..#..##.#...###.#............#."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".....#..........##..#..#####.#..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#....#...#..#.......#...........#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#....#....#....#.#...#.#.#.#...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#..#.....#.......###.#.#.##...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#.##.###..#....#........#..#.#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#.##..#.#.#...##..........#...#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#..#.......................#..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#..#.#...##.#...#.#..#.#......"
            .to_string()
            .as_bytes()
            .to_vec(),
        "......#......#.....#............."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".###..#.#..#...#..#.#.......##..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#...#.................###......#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#.......#..####.#..##.###.....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#.#..#.#...##.#.#..#..##.#.#.#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "##...#....#...#....##....#.#....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "......#..#......#.#.....##..#.#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "##.###.....#.#.###.#..#..#..###.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#...........#.#..#..#..#....#...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "..........#.#.#..#.###...#.....#."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#.###........##..#..##........"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".###.....#.#.###...##.........#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#...##.....#.#.........#..#.###"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..##..##........#........#......#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..####......#...#..........#.#..."
            .to_string()
            .as_bytes()
            .to_vec(),
        "......##...##.#........#...##.##."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#..###...#.......#........#....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "...##...#..#...#..#..#.#.#...#..."
            .to_string()
            .as_bytes()
            .to_vec(),
        "....#......#.#............##....."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#......####...#.....#...#......#."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#............#...#..#.#.#..#.#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#...#....###.####....#.#........"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#...##...#.##...#....#.#..##.#."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#....#.###..#..##.#.##...#.#..##"
            .to_string()
            .as_bytes()
            .to_vec(),
    ];

    let mut max = 0;
    let mut coords = vec![];

    let height = lines.len();
    let width = lines[0].len();
    for y in 0..height {
        for x in 0..width {
            let mut clone = lines.clone();
            if clone[y][x] != b'#' {
                continue;
            }
            clone[y][x] = b'S';
            print_grid(&clone);
            for j in 0..height {
                for i in 0..width {
                    if y == j && x == i {
                        continue;
                    }

                    if [b'#', b'D'].contains(&clone[j][i]) {
                        let raw_ydiff = (j as isize) - (y as isize);
                        let raw_xdiff = (i as isize) - (x as isize);
                        let denom = raw_ydiff.gcd(&raw_xdiff).abs();

                        let ydiff = raw_ydiff / denom;
                        let xdiff = raw_xdiff / denom;

                        let mut xoffset: isize = xdiff;
                        let mut yoffset: isize = ydiff;
                        while (0..(height as isize)).contains(&(yoffset + j as isize))
                            && (0..(width as isize)).contains(&(xoffset + i as isize))
                        {
                            if clone[(yoffset + j as isize) as usize]
                                [(xoffset + i as isize) as usize]
                                == b'#'
                            {
                                clone[(yoffset + j as isize) as usize]
                                    [(xoffset + i as isize) as usize] = b'D';
                            }
                            xoffset += xdiff;
                            yoffset += ydiff;
                        }
                    }
                }
            }
            print_grid(&clone);

            let num_visible = clone
                .into_iter()
                .map(|l| l.into_iter())
                .flatten()
                .filter(|c| *c == b'#')
                .count();
            if max < num_visible {
                max = num_visible;
                coords = vec![x, y];
            }
        }
    }

    println!("{} : {:?}", max, coords);

    Ok(())
}

fn part2() -> Result<(), Box<dyn Error>> {
    let mut lines = vec![
        ".#..#..##.#...###.#............#."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".....#..........##..#..#####.#..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#....#...#..#.......#...........#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#....#....#....#.#...#.#.#.#...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#..#.....#.......###.#.#.##...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#.##.###..#....#........#..#.#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#.##..#.#.#...##..........#...#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..#..#.......................#..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#..#.#...##.#...#.#..#.#......"
            .to_string()
            .as_bytes()
            .to_vec(),
        "......#......#.....#............."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".###..#.#..#...#..#.#.......##..#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#...#.................###......#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#.......#..####.#..##.###.....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#.#..#.#...##.#.#..#..##.#.#.#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "##...#....#...#....##....#.#....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "......#..#......#.#.....##..#.#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "##.###.....#.#.###.#..#..#..###.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#...........#.#..#..#..#....#...."
            .to_string()
            .as_bytes()
            .to_vec(),
        "..........#.#.#..#.###...#.....#."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#.###........##..#..##........"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".###.....#.#.###...##.........#.."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#...##.....#.#.........#..#.###"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..##..##........#........#......#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "..####......#...#..........#.#..."
            .to_string()
            .as_bytes()
            .to_vec(),
        "......##...##.#........#...##.##."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#..###...#.......#........#....#"
            .to_string()
            .as_bytes()
            .to_vec(),
        "...##...#..#...#..#..#.#.#...#..."
            .to_string()
            .as_bytes()
            .to_vec(),
        "....#......#.#............##....."
            .to_string()
            .as_bytes()
            .to_vec(),
        "#......####...#.....#...#......#."
            .to_string()
            .as_bytes()
            .to_vec(),
        "...#............#...#..#.#.#..#.#"
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#...#....###.####....#.#........"
            .to_string()
            .as_bytes()
            .to_vec(),
        "#.#...##...#.##...#....#.#..##.#."
            .to_string()
            .as_bytes()
            .to_vec(),
        ".#....#.###..#..##.#.##...#.#..##"
            .to_string()
            .as_bytes()
            .to_vec(),
    ];

    let height = lines.len();
    let width = lines[0].len();
    let x = 23;
    let y = 29;

    let mut vaporized = 0;

    let mut last_x: isize = -1;
    let mut last_y: isize = -10000000;

    lines[y][x] = b'S';

    loop {
        let mut visible = lines.clone();
        for j in 0..height {
            for i in 0..width {
                if y == j && x == i {
                    continue;
                }

                if [b'#', b'D'].contains(&visible[j][i]) {
                    let raw_ydiff = (j as isize) - (y as isize);
                    let raw_xdiff = (i as isize) - (x as isize);
                    let denom = raw_ydiff.gcd(&raw_xdiff).abs();

                    let ydiff = raw_ydiff / denom;
                    let xdiff = raw_xdiff / denom;

                    let mut xoffset: isize = xdiff;
                    let mut yoffset: isize = ydiff;
                    while (0..(height as isize)).contains(&(yoffset + j as isize))
                        && (0..(width as isize)).contains(&(xoffset + i as isize))
                    {
                        if visible[(yoffset + j as isize) as usize][(xoffset + i as isize) as usize]
                            == b'#'
                        {
                            visible[(yoffset + j as isize) as usize]
                                [(xoffset + i as isize) as usize] = b'D';
                        }
                        xoffset += xdiff;
                        yoffset += ydiff;
                    }
                }
            }
        }
        println!("visible");
        print_grid(&visible);

        let mut closest_x = 0;
        let mut closest_y = 0;
        let mut closest_dist = std::f64::INFINITY;
        for j in 0..height {
            for i in 0..width {
                if b'#' != visible[j][i] {
                    continue;
                }

                let xdiff = (i as isize) - (x as isize);
                let ydiff = (j as isize) - (y as isize);

                let dist = distance((last_x as f64, last_y as f64), (xdiff as f64, ydiff as f64));
                if dist < closest_dist {
                    closest_dist = dist;
                    closest_x = xdiff;
                    closest_y = ydiff;
                }
            }
        }

        last_x = closest_x;
        last_y = closest_y;
        lines[(last_y + y as isize) as usize][(last_x + x as isize) as usize] = b'A';
        println!("vaporized");
        print_grid(&lines);
        lines[(last_y + y as isize) as usize][(last_x + x as isize) as usize] = b'V';
        vaporized += 1;
        if vaporized == 200 {
            print_grid(&lines);
            println!("IT IS {}, {}", last_x + x as isize, last_y + y as isize);
            return Ok(());
        }
    }

    Ok(())
}

fn distance(orig: (f64, f64), other: (f64, f64)) -> f64 {
    distance_n((orig.1).atan2(orig.0), (other.1).atan2(other.0))
}

fn distance_n(orig: f64, other: f64) -> f64 {
    let orig = to_awesome_units(orig);
    let other = to_awesome_units(other);
    if other <= orig {
        (other + 2.) - orig
    } else {
        other - orig
    }
}

fn to_awesome_units(n: f64) -> f64 {
    let n = n / PI;
    if n < 0. {
        2. + n
    } else {
        n
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("{}", distance((0., -1.), (-1., -3.)));
    part2()
}
