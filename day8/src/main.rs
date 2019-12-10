use std::io::{stdin, BufRead, Read};

type Image = Vec<Layer>;
type Layer = Vec<Row>;
type Row = Vec<u8>;

const ROW_LEN: usize = 25;
const COL_LEN: usize = 6;
const LAYER_LEN: usize = ROW_LEN * COL_LEN;

fn make_image(text: &str) -> Image {
    text.as_bytes()
        .chunks_exact(LAYER_LEN)
        .map(|layer| {
            layer
                .chunks_exact(ROW_LEN)
                .map(|row| row.to_vec())
                .collect()
        })
        .collect()
}

fn num_x_digits(layer: &Layer, x: u8) -> usize {
    layer
        .into_iter()
        .map(|r| r.into_iter())
        .flatten()
        .filter(|c| **c == x)
        .count()
}

fn part1(text: &str) {
    let image = make_image(text);

    let min_layer = image
        .into_iter()
        .min_by_key(|layer| num_x_digits(layer, b'0'))
        .unwrap();
    let num1 = num_x_digits(&min_layer, b'1');
    let num2 = num_x_digits(&min_layer, b'2');

    println!("part1: {}", num1 * num2)
}

fn print_layer(img: &Layer) {
    let l: Vec<_> = img
        .iter()
        .map(|r| {
            let l: Vec<_> = r
                .iter()
                .map(|c| *c)
                .map(|c| match c {
                    b'0' => "□",
                    b'1' => "■",
                    _ => "?",
                })
                .map(|s| s.to_string())
                .collect();

            l.join("")
        })
        .collect();
    println!("{}", l.join("\n"));
}

fn part2(text: &str) {
    let mut img = vec![vec![b'2'; ROW_LEN]; COL_LEN];
    for layer in make_image(text).into_iter() {
        for y in 0..img.len() {
            for x in 0..img[0].len() {
                if img[y][x] == b'2' && layer[y][x] != b'2' {
                    img[y][x] = layer[y][x];
                }
            }
        }
    }
    print_layer(&img);
}

fn main() {
    let mut buff = String::new();
    stdin().lock().read_line(&mut buff);

    part1(buff.as_str());
    part2(buff.as_str())
}
