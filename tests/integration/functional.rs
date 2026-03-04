use crate::common::eval_source;
use elle::Value;

#[test]
fn sort_list() {
    let result = eval_source("(sort (list 3 1 2))").unwrap();
    let vec = result.list_to_vec().unwrap();
    assert_eq!(vec, vec![Value::int(1), Value::int(2), Value::int(3)]);
}

#[test]
fn sort_empty_list() {
    assert_eq!(eval_source("(sort ())").unwrap(), Value::EMPTY_LIST);
}

#[test]
fn sort_array_returns_array() {
    let result = eval_source("(array? (sort @[3 1 2]))").unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn sort_tuple_returns_tuple() {
    let result = eval_source("(tuple? (sort [3 1 2]))").unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn sort_type_error_on_non_numbers() {
    assert!(eval_source("(sort (list :a :b))").is_err());
}

#[test]
fn sort_type_error_on_string() {
    assert!(eval_source("(sort \"hello\")").is_err());
}

#[test]
fn range_returns_array() {
    let result = eval_source("(array? (range 5))").unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn range_single_arg() {
    let result = eval_source("(length (range 5))").unwrap();
    assert_eq!(result, Value::int(5));
}

#[test]
fn range_two_args() {
    let result = eval_source("(length (range 2 5))").unwrap();
    assert_eq!(result, Value::int(3));
}

#[test]
fn range_with_step() {
    let result = eval_source("(length (range 0 10 3))").unwrap();
    assert_eq!(result, Value::int(4));
}

#[test]
fn range_negative_step() {
    let result = eval_source("(length (range 5 0 -1))").unwrap();
    assert_eq!(result, Value::int(5));
}

#[test]
fn range_negative_step_wrong_direction() {
    let result = eval_source("(length (range 0 5 -1))").unwrap();
    assert_eq!(result, Value::int(0));
}

#[test]
fn range_zero_step_error() {
    assert!(eval_source("(range 0 5 0)").is_err());
}

#[test]
fn range_type_error() {
    assert!(eval_source("(range :a)").is_err());
}
