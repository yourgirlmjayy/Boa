// runtime/start.rs
// This file provides the entry point for compiled programs

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name on macOS
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here() -> i64;
}

#[no_mangle]
extern "C" fn snek_error(errcode: i64) {
    if errcode == 1 {
        eprintln!("invalid argument");
    } else if errcode == 2 {
        eprintln!("overflow");
    } else {
        eprintln!("an error occurred {}", errcode);
    }
    std::process::exit(1);
}

fn main() {
    let i: i64 = unsafe {
        our_code_starts_here()
    };
    println!("{i}");
}