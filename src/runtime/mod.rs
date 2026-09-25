mod block;

pub use block::{load, Diagnostics, Value, Observer};

#[cfg(feature = "dogfood")]
pub use block::load_observed;
