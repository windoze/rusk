use std::io;
use std::path::{Path, PathBuf};

/// Source text access abstraction used by tools (e.g. LSP) to support unsaved editor buffers.
///
/// The compiler front-end itself remains file-based, but module loading can be redirected to an
/// alternate source (overlay) by implementing this trait.
pub trait SourceProvider: Send + Sync {
    fn read_to_string(&self, path: &Path) -> io::Result<String>;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
    fn exists(&self, path: &Path) -> bool;
}

/// Default filesystem-backed [`SourceProvider`].
#[derive(Clone, Debug, Default)]
pub struct FsSourceProvider;

impl SourceProvider for FsSourceProvider {
    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}
