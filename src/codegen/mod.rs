pub mod body;
pub mod compiler;
pub mod generics;
pub mod layout;
pub mod runtime;
pub mod types;

#[cfg(test)]
mod tests;

#[derive(Debug, Default, PartialEq, Clone)]
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
}

impl std::fmt::Display for SafetyMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Debug => "unoptimized + debug info",
            Self::ReleaseSafe => "safely optimized",
            Self::ReleaseFast => "optimized",
        };
        write!(f, "{str}")
    }
}
