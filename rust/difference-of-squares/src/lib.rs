pub fn square_of_sum(n: u32) -> u32 {
    let mut s: u32 = 0;

    for x in 1..(n+1) {
        s += x;
    }

    s.pow(2)
}

pub fn sum_of_squares(n: u32) -> u32 {
    let mut s: u32 = 0;

    for x in 1..(n+1) {
        s += x.pow(2)
    }

    s
}

pub fn difference(n: u32) -> u32 {
    square_of_sum(n) - sum_of_squares(n)
}
