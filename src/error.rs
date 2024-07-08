use ariadne::Report;

pub struct MerlenError {
    errors: Vec<Report<'static>>,
}

