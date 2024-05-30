mod interpreter;
mod lexer;
mod parser;

extern "C" {
    fn _getch() -> i32;
}

fn main() {
    let mut args = std::env::args().skip(1);
    let mut input_file: Option<String> = None;

    while let Some(arg) = args.next() {
        if let Some(_) = input_file {
            eprintln!("Erro: O compilador só aceita um ficheiro de entrada.");
            println!("\nPressiona qualquer tecla pra sair...");
            unsafe {
                _getch();
            }
            std::process::exit(1);
        } else {
            input_file = Some(arg);
        }
    }

    let input_file = input_file.unwrap_or_else(|| {
        eprintln!("Erro: Faltam ficheiros de entrada.");
        println!("\nPressiona qualquer tecla pra sair...");
        unsafe {
            _getch();
        }
        std::process::exit(1);
    });

    let code = std::fs::read_to_string(input_file).unwrap_or_else(|err| {
        eprintln!("Erro I/O: {err}.");
        println!("\nPressiona qualquer tecla pra sair...");
        unsafe {
            _getch();
        }
        std::process::exit(1);
    });

    let tokens = lexer::tokenize(code).unwrap_or_else(|err| {
        eprintln!("{err}");
        println!("\nPressiona qualquer tecla pra sair...");
        unsafe {
            _getch();
        }
        std::process::exit(1);
    });

    let program = parser::parse(tokens).unwrap_or_else(|err| {
        eprintln!("{err}");
        println!("\nPressiona qualquer tecla pra sair...");
        unsafe {
            _getch();
        }
        std::process::exit(1);
    });

    interpreter::interpret(&program);

    println!("\nPressiona qualquer tecla pra sair...");
    unsafe {
        _getch();
    }
}
