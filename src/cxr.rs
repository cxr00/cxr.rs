use series::ops::Pow;
use std::cmp::{max, min, Ordering};
use std::cmp::Ordering::{Equal, Greater, Less};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Sub, Mul, Index};

struct Number {
    digits: Vec<i32>,
    base: u8,
    sign: bool
}

trait Carry {
    fn resolve(&self) -> Self;
}

trait Negative {
    fn neg(&self) -> Self;
}

impl Clone for Number {
    fn clone(&self) -> Self {
        Number {base: self.base, digits: self.digits.clone(), sign: self.sign}
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}{:?}", self.base, if self.sign {"-"} else {""}, self.digits)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.digits == vec![0] { write!(f, "0") } else {
            let digit_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_".as_bytes();
            let mut output = String::new();
            if self.sign {
                output = format!("-{output}");
            }
            for d in (0..self.digits.len()).rev() {
                output = format!("{output}{}", digit_chars[self[d] as usize] as char);
            }
            write!(f, "{}", output)
        }
    }
}

impl Index<usize> for Number {
    type Output = i32;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.digits.len() { &0 } else { &self.digits[index] }
    }
}

impl PartialEq<Self> for Number {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.digits == other.digits && self.sign == other.sign
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.sign && !other.sign { Some(Less) }
        else if !self.sign && other.sign { Some(Greater) }
        else {
            let reverse: bool = self.sign;
            if self.digits.len() < other.digits.len() {
                if reverse { Some(Greater) } else { Some(Less) }
            } else if self.digits.len() > other.digits.len() {
                if reverse { Some(Less) } else { Some(Greater) }
            } else {
                for d in (0..self.digits.len()).rev() {
                    if self[d] > other[d] {
                        if reverse { Some(Less) } else { Some(Greater) };
                    } else if self[d] < other[d] {
                        if reverse { Some(Greater) } else { Some(Less) };
                    };
                }
                Some(Equal)
            }
        }
    }
}

impl Negative for Number {
    fn neg(&self) -> Self {
        Number {base: self.base, digits: self.digits.clone(), sign: !self.sign}
    }
}

impl Carry for Number {
    fn resolve(&self) -> Self {
        let mut output: Vec<i32> = self.digits.clone();
        let base_as_i32: i32 = self.base as i32;
        for d in 0..output.len()-1 {
            let a = &output[d] % &base_as_i32;
            let b = &output[d] / &base_as_i32;
            output[d] = a;
            output[d+1] += b;
        }
        let mut n = output.len() - 1;
        while output[n] >= base_as_i32 {
            let a = &output[n] % &base_as_i32;
            let b = &output[n] / &base_as_i32;
            output[n] = a;
            output.push(b);
            n += 1;
        }
        while output.len() > 1 && output[output.len() - 1] == 0 {
            output.pop();
        }
        if output == vec![0] { Number {base: self.base, digits: vec![0], sign: false} }
        else if output[output.len() - 1] < 0 {
            for d in 0..output.len() {
                output[d] = -output[d]
            }
            Number { base: self.base, digits: output, sign: !self.sign }.resolve()
        }
        else {
            Number { base: self.base, digits: output, sign: self.sign }
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        assert_eq!(&self.base, &other.base);
        if self.sign && other.sign { (self.neg() + other.neg()).neg() }
        else if self.sign { other - self.neg() }
        else if other.sign { self - other.neg() }
        else {
            let output_length = max(self.digits.len(), other.digits.len());
            let mut output: Vec<i32> = Vec::new();
            for n in 0..output_length {
                output.push(self[n] + other[n]);
            }
            Number { base: self.base, digits: output, sign: false }.resolve()
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        assert_eq!(self.base, other.base);
        if other.sign { return self + other.neg(); }
        let output_length = max(self.digits.len(), other.digits.len());
        let mut output: Vec<i32> = Vec::new();
        for n in 0..output_length {
            output.push(self[n] - other[n]);
        }
        Number {base: self.base, digits: output, sign: self.sign}.resolve()
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        assert_eq!(self.base, other.base);
        let output_sign = self.sign ^ other.sign;
        let len = self.digits.len() + other.digits.len() - 1;
        let mut output = vec![0; len];
        for n in 0..len {
            for k in 0..=n {
                output[n] += self[n-k] * other[k];
            }
        }
        Number {base: self.base, digits: output, sign: output_sign}.resolve()
    }
}

impl Pow<u16> for Number {
    type Output = Self;
    fn pow(self, pow: u16) -> Self {
        let mut output = Number {base: self.base, digits: vec![1], sign: false};
        let base: Number = self.clone();
        for _ in 1..=pow {
            output = output * base.clone();
        }
        output
    }
}

///
/// SIGNATURE LEFT NEAR-RING
///
///

struct Seq {
    elements: Vec<Number>,
    base: u8
}

trait SignatureFunction {
    fn f(&self, length: usize) -> Self;
    fn seeded_f(&self, seed: Self, length: usize) -> Self;
}

trait Length {
    fn len(&self) -> usize;
}

impl Clone for Seq {
    fn clone(&self) -> Self {
        Seq { elements: self.elements.clone(), base: self.base }
    }
}

impl Debug for Seq {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.elements)
    }
}

impl Display for Seq {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // let mut output = String::new();
        let mut output = format!("{}", self[0]);
        for d in 1..self.elements.len() {
            output = format!("{output}, {}", self[d]);
        }
        write!(f, "{}", output)
    }
}

impl Index<usize> for Seq {
    type Output = Number;

    fn index(&self, index: usize) -> &Self::Output {
        // if index >= self.elements.len() { &zero(self.base) }
        &self.elements[index]
    }
}

impl Length for Seq {
    fn len(&self) -> usize {
        self.elements.len()
    }
}

fn zero(base: u8) -> Number {
    Number {base, digits: vec![0], sign: false}
}

fn one(base: u8) -> Number {
    Number {base, digits: vec![1], sign: false}
}

fn x(base: u8) -> Seq {
    Seq {base, elements: vec![zero(base), one(base)]}
}

impl SignatureFunction for Seq {
    fn f(&self, length: usize) -> Self {
        let mut output = vec![Number { base: self.base, digits: vec![1], sign: false }];
        for n in 1..length {
            output.push(Number {base: self.base, digits: vec![0], sign: false } );
            for k in 1..=min(n, self.len()) {
                output[n] = output[n].clone() + output[n-k].clone() * self[k-1].clone();
            }
        }
        Seq { elements: output, base: self.base }
    }
    fn seeded_f(&self, seed: Self, length: usize) -> Self {
        let mut output = seed.elements;
        for n in output.len()..length {
            output.push(Number {base: self.base, digits: vec![0], sign: false } );
            for k in 1..=min(n, self.len()) {
                output[n] = output[n].clone() + output[n-k].clone() * self[k-1].clone();
            }
        }
        Seq { elements: output, base: self.base }
    }
}

impl Add for Seq {
    type Output = Seq;

    fn add(self, other: Self) -> Self::Output {
        let len = max(self.len(), other.len());
        let mut output = vec![Number {base: self.base, digits: vec![0], sign: false}; len];
        for n in 0..len {
            output[n] =
                if n < self.len() {self[n].clone()} else {zero(self.base)} +
                if n < other.len() {other[n].clone()} else {zero(self.base)};
        }
        while output.len() > 1 && output[output.len() - 1].digits == vec![0] {
            output.pop();
        }
        Seq { elements: output, base: self.base }
    }
}

impl Sub for Seq {
    type Output = Seq;

    fn sub(self, other: Self) -> Self::Output {
        let len = max(self.len(), other.len());
        let mut output = vec![Number {base: self.base, digits: vec![0], sign: false}; len];
        for n in 0..len {
            output[n] =
                if n < self.len() {self[n].clone()} else {zero(self.base)} -
                    if n < other.len() {other[n].clone()} else {zero(self.base)};
        }
        while output.len() > 1 && output[output.len() - 1].digits == vec![0] {
            output.pop();
        }
        Seq { elements: output, base: self.base }
    }
}

impl Mul for Seq {
    type Output = Seq;

    fn mul(self, other: Self) -> Self::Output {
        let len = self.len() + other.len() - 1;
        let mut output = vec![Number {base: self.base, digits: vec![0], sign: false}; len];
        for n in 0..len {
            for k in 0..=n {
                output[n] = output[n].clone() +
                    if n-k < self.len() { self[n-k].clone() } else { zero(self.base) } *
                        if k < other.len() { other[k].clone() } else { zero(self.base) };
            }
        }
        while output.len() > 1 && output[output.len() - 1].digits == vec![0] {
            output.pop();
        }
        Seq { elements: output, base: self.base }
    }
}

struct Sig {
    seq: Seq,
    base: u8
}

impl Clone for Sig {
    fn clone(&self) -> Self {
        Sig {base: self.base, seq: self.seq.clone()}
    }
}

impl Debug for Sig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.seq)
    }
}

impl Display for Sig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.seq)
    }
}

impl Length for Sig {
    fn len(&self) -> usize {
        self.seq.len()
    }
}

impl Add for Sig {
    type Output = Sig;

    fn add(self, other: Sig) -> Sig {
        Sig {base: self.base, seq: self.seq.clone() + other.seq.clone() - self.seq*other.seq*x(self.base) }
    }
}

impl Sub for Sig {
    type Output = Sig;

    fn sub(self, other: Sig) -> Sig {
        let diff = self.len() + other.len() - 1;
        let arith = (self.seq.clone() - other.seq.clone()) * other.seq.clone().f(diff + 10);
        let mut output = vec![zero(self.base); arith.len() - diff];
        for (i, elem) in (0..output.len()).enumerate() {
            output[i] = arith[elem].clone();
        }
        while output.len() > 1 && output[output.len() - 1] == zero(self.base) {
            output.pop();
        }
        Sig {base: self.base, seq: Seq {base: self.base, elements: output } }
    }
}

impl Mul for Sig {
    type Output = Sig;

    fn mul(self, other: Sig) -> Sig {
        let mut output = Seq {base: self.base, elements: vec![other.seq[0].clone()]};
        let mut g = Seq {base: self.base, elements: vec![one(self.base)]};
        let ax = self.seq.clone() * x(self.base);

        for k in 1..other.len() {
            g = g.clone() * ax.clone();
            output = output.clone() + g.clone() * Seq { base: self.base, elements: vec![other.seq[k].clone()] };
        }
        output = output.clone() * self.seq.clone();
        Sig {base: self.base, seq: output}
    }
}

fn main(){
    let base = 10;
    let n = Number {base, digits: vec![1], sign: false};
    let two = Number {base, digits: vec![2], sign: false};
    let seq = Seq {elements: vec![n.clone(), n.clone()], base};
    let seed = Seq {elements: vec![two, n], base};
    println!("{}", seq.f(500));
    println!("{}", seq.seeded_f(seed.clone(), 12));
    println!("{}", (seq.clone() * seq.clone() * seq.clone()).seeded_f(seed.clone(),25));
    let sig = Sig {seq: seq.clone(), base};
    println!("{}", sig.clone() + sig.clone());
    println!("{}", (sig.clone() * sig.clone()) + sig.clone());
}
