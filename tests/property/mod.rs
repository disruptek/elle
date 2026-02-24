// Property-based tests harness
mod strategies;
mod coroutines {
    include!("coroutines.rs");
}
mod bugfixes {
    include!("bugfixes.rs");
}
mod fibers {
    include!("fibers.rs");
}
mod macros {
    include!("macros.rs");
}
mod destructuring {
    include!("destructuring.rs");
}
mod nanboxing {
    include!("nanboxing.rs");
}
