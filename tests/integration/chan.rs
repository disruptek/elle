use crate::common::eval_source;
use elle::Value;

#[test]
fn test_chan_new_unbounded() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (tuple? [s r]))
        "#,
    );
    assert!(result.is_ok(), "chan/new should work: {:?}", result);
}

#[test]
fn test_chan_new_bounded() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new 10)))
          (tuple? [s r]))
        "#,
    );
    assert!(
        result.is_ok(),
        "chan/new with capacity should work: {:?}",
        result
    );
}

#[test]
fn test_chan_send_ok() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/send s 42))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("ok"));
}

#[test]
fn test_chan_recv_ok() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/send s 42)
          (chan/recv r))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 2);
    assert_eq!(elems[0].as_keyword_name(), Some("ok"));
    assert_eq!(elems[1], Value::int(42));
}

#[test]
fn test_chan_recv_empty() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/recv r))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("empty"));
}

#[test]
fn test_chan_send_full() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new 1)))
          (chan/send s 1)
          (chan/send s 2))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("full"));
}

#[test]
fn test_chan_disconnected_send() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/close-recv r)
          (chan/send s 42))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("disconnected"));
}

#[test]
fn test_chan_disconnected_recv() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/close s)
          (chan/recv r))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("disconnected"));
}

#[test]
fn test_chan_clone() {
    let result = eval_source(
        r#"
        (let* (([s r] (chan/new))
               (s2 (chan/clone s)))
          (chan/send s 1)
          (chan/send s2 2)
          (let* ((r1 (chan/recv r))
                 (r2 (chan/recv r)))
            [r1 r2]))
        "#,
    )
    .unwrap();

    let outer = result.as_tuple().expect("should be a tuple");
    assert_eq!(outer.len(), 2);

    let r1 = outer[0].as_tuple().expect("r1 should be a tuple");
    assert_eq!(r1[0].as_keyword_name(), Some("ok"));
    assert_eq!(r1[1], Value::int(1));

    let r2 = outer[1].as_tuple().expect("r2 should be a tuple");
    assert_eq!(r2[0].as_keyword_name(), Some("ok"));
    assert_eq!(r2[1], Value::int(2));
}

#[test]
fn test_chan_keyword_messages() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/send s :empty)
          (chan/send s :full)
          (chan/send s :ok)
          (chan/send s :disconnected)
          (let* ((r1 (chan/recv r))
                 (r2 (chan/recv r))
                 (r3 (chan/recv r))
                 (r4 (chan/recv r)))
            [r1 r2 r3 r4]))
        "#,
    )
    .unwrap();

    let outer = result.as_tuple().expect("should be a tuple");
    assert_eq!(outer.len(), 4);

    for (i, expected_kw) in ["empty", "full", "ok", "disconnected"].iter().enumerate() {
        let recv_result = outer[i].as_tuple().expect("should be a tuple");
        assert_eq!(
            recv_result.len(),
            2,
            "recv result {} should have 2 elements",
            i
        );
        assert_eq!(
            recv_result[0].as_keyword_name(),
            Some("ok"),
            "recv result {} status should be :ok",
            i
        );
        assert_eq!(
            recv_result[1].as_keyword_name(),
            Some(*expected_kw),
            "recv result {} message should be :{}",
            i,
            expected_kw
        );
    }
}

#[test]
fn test_chan_select_ready() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/send s 99)
          (chan/select @[r] 1000))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 2);
    assert_eq!(elems[0], Value::int(0));
    assert_eq!(elems[1], Value::int(99));
}

#[test]
fn test_chan_select_timeout() {
    let result = eval_source(
        r#"
        (let (([s r] (chan/new)))
          (chan/select @[r] 10))
        "#,
    )
    .unwrap();

    let elems = result.as_tuple().expect("should be a tuple");
    assert_eq!(elems.len(), 1);
    assert_eq!(elems[0].as_keyword_name(), Some("timeout"));
}
