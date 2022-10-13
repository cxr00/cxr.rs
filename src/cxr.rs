use std::cmp::{max, min, Ordering};
use std::cmp::Ordering::{Equal, Greater, Less};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Sub, Mul, Index};
use std::time;

struct Number {
    base: u8,
    digits: Vec<i32>,
    sign: bool
}

trait Carry {
    fn resolve(&self) -> Self;
}

trait Negative {
    fn neg(&self) -> Self;
}

trait Convert {
    fn convert(self, base: u8) -> Self;
}

impl Clone for Number {
    fn clone(&self) -> Self {
        Number {base: self.base, digits: self.digits.clone(), sign: self.sign}
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Number")
            .field("base", &self.base)
            .field("digits", &self.digits)
            .field("sign", &self.sign)
            .finish()
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
                return if reverse { Some(Greater) } else { Some(Less) }
            } else if self.digits.len() > other.digits.len() {
                return if reverse { Some(Less) } else { Some(Greater) }
            } else {
                for d in (0..self.digits.len()).rev() {
                    if self[d] > other[d] {
                        return if reverse { Some(Less) } else { Some(Greater) }
                    } else if self[d] < other[d] {
                        return if reverse { Some(Greater) } else { Some(Less) }
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
            let a = if output[d] < 0 {&base_as_i32 + &output[d]} else {&output[d] % &base_as_i32};
            let b = if output[d] < 0 {(&output[d] / &base_as_i32) - 1 } else {&output[d] / &base_as_i32};
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

impl Convert for Number {
    fn convert(self, base: u8) -> Self {
        if base == self.base {
            self
        } else {
            assert!(64 >= base && base >= 2);
            let mut self_clone = Number {base: self.base, digits: self.digits.clone(), sign: false};
            let mut mod_powers = vec![Number { base: self.base, digits: vec![1], sign: false}];
            let exp_base = Number {base: self.base, digits: vec![base.clone() as i32], sign: false};
            let mut current_power: usize = 0;
            loop {
                let temp = mod_powers[mod_powers.len() - 1].clone() * exp_base.clone();
                if temp > self_clone {
                    break;
                } else {
                    mod_powers.push(temp);
                    current_power += 1;
                }
            }
            let mut output_vec = vec![0; (current_power + 1) as usize];
            while current_power > 0 {
                self_clone = self_clone - mod_powers[current_power].clone();
                output_vec[current_power] += 1;
                if output_vec[current_power] > 10 { break };
                while mod_powers[current_power] > self_clone {
                    current_power -= 1;
                }
            }
            output_vec[0] = self_clone[0];
            Number {base, digits: output_vec, sign: self.sign}
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if self.base != other.base { self.clone() + other.convert(self.base) }
        else if self.sign && other.sign { (self.neg() + other.neg()).neg() }
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
        if self.base != other.base { return self.clone() - other.convert(self.base); }
        else if other.sign { return self + other.neg(); }
        let output_length = max(self.digits.len(), other.digits.len());
        let mut output: Vec<i32> = Vec::new();
        for n in 0..output_length {
            output.push(self[n] - other[n]);
        }
        Number {base: self.base, digits: output, sign: self.sign}.resolve()
    }
}

fn __school_mul__(number_a: Number, number_b: Number) -> Number {
    if number_a.base != number_b.base { __school_mul__(number_b.convert(number_a.base), number_a) }
    else {
        let output_sign = number_a.sign ^ number_b.sign;
        let len = number_a.digits.len() + number_b.digits.len() - 1;
        let mut output = vec![0; len];
        for n in 0..len {
            for k in 0..=n {
                output[n] += number_a[n - k] * number_b[k];
            }
        }
        Number { base: number_a.base, digits: output, sign: output_sign }.resolve()
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        if self.base != other.base { other.convert(self.base) * self } else {
            let output_sign = self.sign ^ other.sign;
            if self.digits.len() < 50 && other.digits.len() < 50 {
                __school_mul__(self, other)
            } else {
                let m = min(self.digits.len(), other.digits.len());
                if m == 1 {
                    let g = if self.digits.len() != 1 { self.clone() } else { other.clone() };
                    let p = if g != self { self.clone() } else { other.clone() };
                    let p = p[0];
                    let mut r: Vec<i32> = vec![0; g.digits.len()];
                    for (i, e) in g.digits.iter().enumerate() {
                        r[i] = p.clone() * e;
                    }
                    Number { base: self.base, digits: r, sign: output_sign }.resolve()
                } else {
                    let m = m / 2;
                    let x0 = Number { base: self.base, digits: self.digits[..m].to_vec(), sign: self.sign };
                    let x1 = Number { base: self.base, digits: self.digits[m..].to_vec(), sign: self.sign };
                    let y0 = Number { base: other.base, digits: other.digits[..m].to_vec(), sign: other.sign };
                    let y1 = Number { base: other.base, digits: other.digits[m..].to_vec(), sign: other.sign };

                    if m < 25 {
                        let z2 = __school_mul__(x1.clone(), y1.clone());
                        let z0 = __school_mul__(x0.clone(), y0.clone());
                        let z1 = __school_mul__(x1 + x0, y1 + y0) - z2.clone() - z0.clone();
                        let mut pt0 = vec![0; m * 2];
                        pt0.extend(z2.digits);
                        let pt0 = Number { base: self.base, digits: pt0, sign: output_sign };
                        let mut pt1 = vec![0; m];
                        pt1.extend(z1.digits);
                        let pt1 = Number { base: self.base, digits: pt1, sign: output_sign };
                        (pt0 + pt1 + z0).resolve()
                    } else {
                        let z2 = x1.clone() * y1.clone();
                        let z0 = x0.clone() * y0.clone();
                        let z1 = (x1 + x0) * (y1 + y0) - z2.clone() - z0.clone();
                        let mut pt0 = vec![0; m * 2];
                        pt0.extend(z2.digits);
                        let pt0 = Number { base: self.base, digits: pt0, sign: output_sign };
                        let mut pt1 = vec![0; m];
                        pt1.extend(z1.digits);
                        let pt1 = Number { base: self.base, digits: pt1, sign: output_sign };
                        (pt0 + pt1 + z0).resolve()
                    }
                }
            }
        }
    }
}

impl Number {
    fn pow(self, pow: u16) -> Self {
        let mut output = Number {base: self.base, digits: vec![1], sign: false};
        let base: Number = self;
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
    base: u8,
    elements: Vec<Number>
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
        let mut elem = String::new();
        for (i, e) in self.elements.iter().enumerate() {
            elem = elem + &*format!("{}{}", e, if i == self.len() - 1 { "" } else { ", " });
        }
        write!(f, "Seq {{ base: {}, elements: {} }}", self.base, elem)
    }
}

impl Display for Seq {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
        write!(f, "Sig {{ {:?} }}", self.seq)
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
        output = output * self.seq;
        Sig {base: self.base, seq: output}
    }
}

fn main(){
    let base = 10;
    let n = Number {base, digits: vec![1], sign: false};
    let two = Number {base, digits: vec![2], sign: false};
    let seq = Seq {elements: vec![n.clone(), n.clone(), n.clone()], base};
    let seed = Seq {elements: vec![two.clone(), n], base};
    println!("{:?}", two.clone().pow(12));
    println!("{:?}", seq.f(500));
    println!("{}", seq.seeded_f(seed.clone(), 12));
    println!("{}", (seq.clone() * seq.clone() * seq.clone()).seeded_f(seed.clone(),25));
    let sig = Sig {seq: seq.clone(), base};
    println!("{:?}", sig.clone() + sig.clone());
    println!("{}", (sig.clone() * sig.clone()) + sig.clone());
    let start = time::Instant::now();
    println!("{}", (two.clone().pow(7000) * two.clone().pow(5000)));
    println!("{}", start.elapsed().as_micros());
    println!("{}", seq * seed);

    println!("{}", Number {base, digits: vec![-1, 1], sign: false}.resolve());

    let num1 = Number {base, digits: vec![2,1,3,1], sign: false};
    println!("{}", num1.clone() - num1.clone() * Number {base: 10, digits: vec![2], sign: false});
    println!("{}", num1.convert(7));
    let num1 = Number {base: 5, digits: vec![1], sign: false};
    let num2 = Number {base: 6, digits: vec![1], sign: false};
    println!("{}", num1 + num2);
}
