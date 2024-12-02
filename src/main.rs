use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, WrapErr};
use scheme::*;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    // Run { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                println!("{:?}", token);
            }
        }
        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let mut parser = parser::Parser::new(&file_contents);
            match parser.parse() {
                Ok(tt) => println!("{tt}"),
                Err(e) => {
                    eprintln!("{e:?}");
                }
            }
        }
    }

    Ok(())
}
