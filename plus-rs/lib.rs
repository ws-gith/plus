#[cfg(feature = "axum")]
pub use axum;

#[cfg(feature = "std-rs")]
pub use std_rs;

#[cfg(test)]
mod test {
    use serde_json::to_string;
    use std_rs::Sensitive;

    #[test]
    fn use_sensitive_serde() {
        let password = Sensitive::new("V337@steyw");
        let sensitive_json = to_string(&password);

        println!("{}", password);
        println!("{:?}", sensitive_json);
    }
}
