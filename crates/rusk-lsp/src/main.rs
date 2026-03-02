use rusk_lsp::{init_tracing, run_stdio_server};

#[tokio::main]
async fn main() {
    init_tracing();

    let config = match rusk_lsp::config::parse_cli_args(std::env::args_os()) {
        Ok(cfg) => cfg,
        Err(msg) => {
            // `--help` / `--version` use the same error channel for a compact CLI parser.
            if msg.starts_with("usage:") || msg.starts_with("rusk-lsp ") {
                println!("{msg}");
                return;
            }
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };

    if let Err(err) = run_stdio_server(config).await {
        eprintln!("rusk-lsp: {err}");
        std::process::exit(1);
    }
}
