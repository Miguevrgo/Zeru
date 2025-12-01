use std::fs;
use std::io::Write;

struct Vector3 {
    x: f32,
    y: f32,
    z: f32,
}

fn create_vector(x: f32, y: f32) -> Vector3 {
    Vector3 { x, y, z: 0.0 }
}

fn main() {
    let pos = create_vector(10.5, 20.0);
    let numbers: [i32; 3] = [1, 2, 3];
    let mut dyn_numbers = Vec::<u32>::new();
    let mut dyn_numbers_explicit: Vec<i32> = Vec::new();

    let mut count = 100;

    if pos.x < 50.0 {
        print!("Position is small: {}\n", pos.x);
    } else if pos.x > 100.0 {
        println!("Position is big: {}", pos.x * 2.0);
    } else {
        eprintln!("Position should not be inside [50-100]");
    }

    while count > 0 {
        count = count - 1;
        count -= (count as f32).clamp(0.0, 200.0) as i32;
    }

    let file_content = fs::read_to_string("input.txt").unwrap();
    let mut output_file = fs::File::create("output.txt").unwrap();
    let lines: Vec<&str> = file_content.split('\n').collect();

    for line in lines {
        if line.contains('=') {
            let parts = line.split_once('=').unwrap();

            // Rust parse returns Result, we convert to Option to match Zeru logic
            let key = parts.1.trim().parse::<i32>().ok();

            if key.is_some() {
                write!(output_file, "{}", key.unwrap()).unwrap();
            }
        }
    }
}
