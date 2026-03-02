#![forbid(unsafe_code)]

pub mod config;
pub mod server;
pub mod text;

pub use config::RuskLspConfig;
pub use server::RuskLanguageServer;

pub fn init_tracing() {
    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info"));
    let _ = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .try_init();
}

pub async fn run_stdio_server(config: RuskLspConfig) -> Result<(), Box<dyn std::error::Error>> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        tower_lsp::LspService::new(|client| RuskLanguageServer::new(client, config.clone()));
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
    Ok(())
}
