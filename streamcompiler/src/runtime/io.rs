
#[no_mangle]
pub extern "C" fn print_double(value: f64) {
    print!("{} ", value);
}

#[no_mangle]
pub extern "C" fn print_double_newline(value: f64) {
    println!("{}", value);
}