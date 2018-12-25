pub fn find() -> Option<u32> {
    for a in 1..1000u32 {
        for b in 1..1000u32 {
            for c in 1..1000u32 {
                if a.pow(2) + b.pow(2) == c.pow(2) {
                    if a + b + c == 1000 {
                        println!("{} {} {}", a, b, c);
                        return Some(a * b * c);
                    }
                }
            }
        }
    }
    None
}

