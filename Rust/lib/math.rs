use std::iter::{Product, Sum};

use num::{traits::Euclid, Integer, Signed};

/// Calculate the greatest common divisor and the Bézout coefficients using the
/// extended Euclidean algorithm.
///
/// #### Example
/// ```
/// # use aoc::math::ext_gcd;
/// let (a, b) = (15, 25);
/// let (gcd, x, y) = ext_gcd(a, b);
/// assert_eq!(gcd, 5);
/// assert_eq!(a * x + b * y, gcd); // Bézout's identity
///
/// // More solutions for Bézout's identity
/// let (x1, y1) = (x + y / gcd * 1, y + x / gcd * 1);
/// let (x2, y2) = (x + y / gcd * 2, y + x / gcd * 2);
/// assert_eq!(a * x1 + b * y1, gcd);
/// assert_eq!(a * x2 + b * y2, gcd);
/// ```
pub fn ext_gcd<T: Integer + Copy>(a: T, b: T) -> (T, T, T) {
    let (mut old_r, mut r) = (a, b);
    let (mut old_s, mut s) = (T::one(), T::zero());
    let (mut old_t, mut t) = (T::zero(), T::one());
    while !r.is_zero() {
        let q = old_r / r;
        (old_r, r) = (r, old_r % r);
        (old_s, s) = (s, old_s - q * s);
        (old_t, t) = (t, old_t - q * t);
    }
    (old_r, old_s, old_t)
}

/// Solve a system of congruences using the Chinese remainder theorem.
///
/// #### Example
/// ```
/// # use aoc::math::chinese_remainder;
/// let (x, n) = chinese_remainder(&[
///     (0, 3), // x % 3 == 0
///     (3, 4), // x % 4 == 3
///     (4, 5), // x % 5 == 4
/// ]);
/// assert_eq!(x, 39);
/// assert_eq!(n, 60);
///
/// // More solutions
/// let x1 = x + n * 1;
/// assert_eq!(x1 % 3, 0);
/// assert_eq!(x1 % 4, 3);
/// assert_eq!(x1 % 5, 4);
///
/// let x2 = x + n * 2;
/// assert_eq!(x2 % 3, 0);
/// assert_eq!(x2 % 4, 3);
/// assert_eq!(x2 % 5, 4);
/// ```
pub fn chinese_remainder<T: Integer + Copy + Sum + Product + Euclid + Signed>(
    congruences: &[(T, T)],
) -> (T, T) {
    debug_assert!(
        congruences.iter().all(|(_, n)| n.is_positive()),
        "moduli must be positive"
    );
    debug_assert!(
        congruences
            .iter()
            .enumerate()
            .all(|(i, (_, n1))| congruences
                .iter()
                .take(i)
                .all(|(_, n2)| n1.gcd(n2).is_one())),
        "moduli must be pairwise coprime"
    );

    let n = congruences.iter().copied().map(|(_, n_)| n_).product();
    let x = congruences
        .iter()
        .copied()
        .map(|(a_, n_)| {
            let ni = n / n_;
            a_ * ext_gcd(ni, n_).1 * ni
        })
        .sum::<T>()
        .rem_euclid(&n);
    (x, n)
}

#[cfg(test)]
mod tests {
    use proptest::{collection, prop_assert_eq, prop_assume, proptest, strategy::Strategy};

    use super::*;

    #[test]
    fn test_ext_gcd() {
        assert_eq!(ext_gcd(240, 46), (2, -9, 47));
        assert_eq!(ext_gcd(432, 126), (18, -2, 7));
    }

    #[test]
    fn test_chinese_remainder() {
        assert_eq!(chinese_remainder(&[(0, 3), (3, 4), (4, 5)]), (39, 60));
    }

    proptest! {
        #[test]
        fn proptest_ext_gcd(a in (-1000i32..=1000), b in (-1000i32..=1000)) {
            prop_assume!(a != 0 || b != 0);
            let (gcd, x, y) = ext_gcd(a, b);
            prop_assert_eq!(a % gcd, 0, "a not divisible by gcd");
            prop_assert_eq!(b % gcd, 0, "b not divisible by gcd");
            prop_assert_eq!(a * x + b * y, gcd, "Bézout's identity does not hold");
        }

        #[test]
        fn proptest_chinese_remainder(
            congruences in collection::vec(-1000i64..=1000, 5),
            moduli in collection::vec(1i64..=1000, 5)
                .prop_filter("moduli must be pairwise coprime",
                    |congruences| congruences
                        .iter()
                        .enumerate()
                        .all(|(i, n1)| congruences
                            .iter()
                            .take(i)
                            .all(|n2| n1.gcd(n2) == 1))
                )
        ) {
            let congruences = congruences.into_iter().zip(moduli).collect::<Vec<_>>();
            let (x, _) = chinese_remainder(&congruences);
            for (a, n) in &congruences {
                prop_assert_eq!((x - a).rem_euclid(*n), 0);
            }
        }
    }
}
