pub use ariadne::Color;

pub enum DiagnosticKind {
    Error,
    Warning,
}

pub struct DiagnosticLabel {
    msg: Box<str>,
    attached_to: Span,
    color: Option<Color>,
    order: i32,
    priority: i32,
}

pub struct Diagnostic {
    kind: DiagnosticKind,
    labels: Vec<DiagnosticLabel>,
}
