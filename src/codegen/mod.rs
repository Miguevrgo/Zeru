pub mod body;
pub mod compiler;
pub mod generics;
pub mod layout;
pub mod runtime;
pub mod types;

#[cfg(test)]
mod tests;

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub enum SafetyMode {
    #[default]
    Debug,
    ReleaseSafe,
    ReleaseFast,
}

impl SafetyMode {
    pub const fn emit_safety_checks(&self) -> bool {
        matches!(self, SafetyMode::Debug | SafetyMode::ReleaseSafe)
    }

    pub const fn from_flags(release_fast: bool, release_safe: bool) -> Self {
        if release_fast {
            Self::ReleaseFast
        } else if release_safe {
            Self::ReleaseSafe
        } else {
            Self::Debug
        }
    }

    pub const fn clang_flags(&self) -> &[&str] {
        match self {
            Self::Debug => &["-O0", "-g"],
            Self::ReleaseSafe => &["-O2"],
            Self::ReleaseFast => &["-O3"],
        }
    }
}

impl std::fmt::Display for SafetyMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Debug => "unoptimized + debug info",
            Self::ReleaseSafe => "safely optimized",
            Self::ReleaseFast => "optimized",
        })
    }
}
