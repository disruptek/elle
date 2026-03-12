// Unit tests for effect combine laws and effect predicates.
//
// Verifies that the effect system satisfies algebraic laws:
// - Signal combine is commutative, associative, and idempotent
// - Signal::inert() is the identity element
// - Propagates field is correctly ORed during combine
// - Signal predicates (may_yield, may_error, may_suspend, etc.) work correctly
// Converted from property tests to deterministic unit tests with concrete cases.

use elle::signals::Signal;
use elle::value::SignalBits;

// =========================================================================
// Signal combine laws: commutativity
// =========================================================================

#[test]
fn effect_combine_commutative_none_none() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    assert_eq!(a.combine(b), b.combine(a));
}

#[test]
fn effect_combine_commutative_1_2() {
    let a = Signal {
        bits: SignalBits(1),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(2),
        propagates: 0,
    };
    assert_eq!(a.combine(b), b.combine(a));
}

#[test]
fn effect_combine_commutative_3_5() {
    let a = Signal {
        bits: SignalBits(3),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(5),
        propagates: 0,
    };
    assert_eq!(a.combine(b), b.combine(a));
}

#[test]
fn effect_combine_commutative_7_7() {
    let a = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(a.combine(b), b.combine(a));
}

// =========================================================================
// Signal combine laws: associativity
// =========================================================================

#[test]
fn effect_combine_associative_none_none_none() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    let c = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    assert_eq!(a.combine(b).combine(c), a.combine(b.combine(c)));
}

#[test]
fn effect_combine_associative_1_2_4() {
    let a = Signal {
        bits: SignalBits(1),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(2),
        propagates: 0,
    };
    let c = Signal {
        bits: SignalBits(4),
        propagates: 0,
    };
    assert_eq!(a.combine(b).combine(c), a.combine(b.combine(c)));
}

#[test]
fn effect_combine_associative_3_5_7() {
    let a = Signal {
        bits: SignalBits(3),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(5),
        propagates: 0,
    };
    let c = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(a.combine(b).combine(c), a.combine(b.combine(c)));
}

#[test]
fn effect_combine_associative_all_same() {
    let a = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    let c = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(a.combine(b).combine(c), a.combine(b.combine(c)));
}

// =========================================================================
// Signal combine laws: identity
// =========================================================================

#[test]
fn effect_combine_identity_none_right() {
    let e = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    assert_eq!(e.combine(Signal::inert()), e);
}

#[test]
fn effect_combine_identity_none_left() {
    let e = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    assert_eq!(Signal::inert().combine(e), e);
}

#[test]
fn effect_combine_identity_1_right() {
    let e = Signal {
        bits: SignalBits(1),
        propagates: 0,
    };
    assert_eq!(e.combine(Signal::inert()), e);
}

#[test]
fn effect_combine_identity_1_left() {
    let e = Signal {
        bits: SignalBits(1),
        propagates: 0,
    };
    assert_eq!(Signal::inert().combine(e), e);
}

#[test]
fn effect_combine_identity_7_right() {
    let e = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(e.combine(Signal::inert()), e);
}

#[test]
fn effect_combine_identity_7_left() {
    let e = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(Signal::inert().combine(e), e);
}

#[test]
fn effect_combine_identity_15_right() {
    let e = Signal {
        bits: SignalBits(15),
        propagates: 0,
    };
    assert_eq!(e.combine(Signal::inert()), e);
}

#[test]
fn effect_combine_identity_15_left() {
    let e = Signal {
        bits: SignalBits(15),
        propagates: 0,
    };
    assert_eq!(Signal::inert().combine(e), e);
}

// =========================================================================
// Signal combine laws: idempotence
// =========================================================================

#[test]
fn effect_combine_idempotent_none() {
    let e = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

#[test]
fn effect_combine_idempotent_1() {
    let e = Signal {
        bits: SignalBits(1),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

#[test]
fn effect_combine_idempotent_3() {
    let e = Signal {
        bits: SignalBits(3),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

#[test]
fn effect_combine_idempotent_5() {
    let e = Signal {
        bits: SignalBits(5),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

#[test]
fn effect_combine_idempotent_7() {
    let e = Signal {
        bits: SignalBits(7),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

#[test]
fn effect_combine_idempotent_15() {
    let e = Signal {
        bits: SignalBits(15),
        propagates: 0,
    };
    assert_eq!(e.combine(e), e);
}

// =========================================================================
// Signal propagates: OR combination
// =========================================================================

#[test]
fn effect_propagates_combine_none_none() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 0,
    };
    let combined = a.combine(b);
    assert_eq!(combined.propagates, 0);
}

#[test]
fn effect_propagates_combine_1_2() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 1,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 2,
    };
    let combined = a.combine(b);
    assert_eq!(combined.propagates, 1 | 2);
}

#[test]
fn effect_propagates_combine_128_255() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 128,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 255,
    };
    let combined = a.combine(b);
    assert_eq!(combined.propagates, 128 | 255);
}

#[test]
fn effect_propagates_combine_same() {
    let a = Signal {
        bits: SignalBits(0),
        propagates: 42,
    };
    let b = Signal {
        bits: SignalBits(0),
        propagates: 42,
    };
    let combined = a.combine(b);
    assert_eq!(combined.propagates, 42 | 42);
}

// =========================================================================
// Polymorphic effects
// =========================================================================

#[test]
fn polymorphic_effect_is_polymorphic_0() {
    let effect = Signal::polymorphic(0);
    assert!(effect.is_polymorphic());
    assert!(effect.may_suspend());
}

#[test]
fn polymorphic_effect_is_polymorphic_1() {
    let effect = Signal::polymorphic(1);
    assert!(effect.is_polymorphic());
    assert!(effect.may_suspend());
}

#[test]
fn polymorphic_effect_is_polymorphic_7() {
    let effect = Signal::polymorphic(7);
    assert!(effect.is_polymorphic());
    assert!(effect.may_suspend());
}

#[test]
fn polymorphic_propagates_correct_param_0() {
    let effect = Signal::polymorphic(0);
    let propagated: Vec<_> = effect.propagated_params().collect();
    assert_eq!(propagated.len(), 1);
    assert_eq!(propagated[0], 0);
}

#[test]
fn polymorphic_propagates_correct_param_1() {
    let effect = Signal::polymorphic(1);
    let propagated: Vec<_> = effect.propagated_params().collect();
    assert_eq!(propagated.len(), 1);
    assert_eq!(propagated[0], 1);
}

#[test]
fn polymorphic_propagates_correct_param_7() {
    let effect = Signal::polymorphic(7);
    let propagated: Vec<_> = effect.propagated_params().collect();
    assert_eq!(propagated.len(), 1);
    assert_eq!(propagated[0], 7);
}

#[test]
fn polymorphic_errors_has_error_bit_0() {
    let effect = Signal::polymorphic_errors(0);
    assert!(effect.may_error());
    assert!(effect.is_polymorphic());
}

#[test]
fn polymorphic_errors_has_error_bit_1() {
    let effect = Signal::polymorphic_errors(1);
    assert!(effect.may_error());
    assert!(effect.is_polymorphic());
}

#[test]
fn polymorphic_errors_has_error_bit_7() {
    let effect = Signal::polymorphic_errors(7);
    assert!(effect.may_error());
    assert!(effect.is_polymorphic());
}

// =========================================================================
// Signal predicates
// =========================================================================

#[test]
fn none_effect_is_not_yielding() {
    let effect = Signal::inert();
    assert!(!effect.may_yield());
    assert!(!effect.may_ffi());
    assert!(!effect.may_suspend());
}

#[test]
fn yields_effect_may_yield() {
    let effect = Signal::yields();
    assert!(effect.may_yield());
    assert!(effect.may_suspend());
}

#[test]
fn errors_effect_may_error() {
    let effect = Signal::errors();
    assert!(effect.may_error());
    assert!(!effect.may_yield());
}

#[test]
fn yields_errors_has_both() {
    let effect = Signal::yields_errors();
    assert!(effect.may_yield());
    assert!(effect.may_error());
    assert!(effect.may_suspend());
}

#[test]
fn ffi_effect_may_ffi() {
    let effect = Signal::ffi();
    assert!(effect.may_ffi());
}

#[test]
fn halts_effect_may_halt() {
    let effect = Signal::halts();
    assert!(effect.may_halt());
    assert!(effect.may_error());
}
