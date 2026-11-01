//! std.simd calls, typed here rather than by their declarations: a vector
//! is an array of exactly the target's vector bytes, and what each call
//! gives follows from its arguments.

use crate::ast::{Expression, ExpressionKind};
use crate::errors::Span;
use crate::tir::{Builtin, Expr, ExprKind, SimdOp};
use crate::ty::{IntTy, Ty, TyKind, Types};

use super::body::FnCx;

/// The operation a std.simd function name stands for.
pub(super) fn simd_op(name: &str) -> Option<SimdOp> {
    Some(match name {
        "load" => SimdOp::Load,
        "store" => SimdOp::Store,
        "add" => SimdOp::Add,
        "sub" => SimdOp::Sub,
        "mul" => SimdOp::Mul,
        "min" => SimdOp::Min,
        "max" => SimdOp::Max,
        "and" => SimdOp::And,
        "or" => SimdOp::Or,
        "xor" => SimdOp::Xor,
        "shl" => SimdOp::Shl,
        "shr" => SimdOp::Shr,
        "eq" => SimdOp::Eq,
        "lt" => SimdOp::Lt,
        "gt" => SimdOp::Gt,
        "select" => SimdOp::Select,
        "sum" => SimdOp::Sum,
        "madd" => SimdOp::Madd,
        _ => return None,
    })
}

impl FnCx<'_, '_> {
    /// The bits of a lane of `elem`, a number type.
    fn lane_bits(&self, elem: Ty) -> Option<u32> {
        match self.cx.types.kind(elem) {
            TyKind::Int(int) => Some(int.bits()),
            TyKind::F32 => Some(32),
            TyKind::F64 => Some(64),
            _ => None,
        }
    }

    /// The vector type of lanes of `elem`.
    fn vector_of(&mut self, elem: Ty) -> Ty {
        let bits = self.lane_bits(elem).expect("a number");
        let lanes = self.cx.simd_bytes * 8 / u64::from(bits);
        self.cx.types.intern(TyKind::Array(elem, lanes))
    }

    /// The lane type of `ty` when it is a vector.
    fn lanes_of(&mut self, ty: Ty) -> Option<Ty> {
        let TyKind::Array(elem, _) = *self.cx.types.kind(ty) else {
            return None;
        };
        (self.lane_bits(elem).is_some() && self.vector_of(elem) == ty).then_some(elem)
    }

    /// The signed integer vector as wide, lane by lane, as vectors of `elem`.
    fn mask_of(&mut self, elem: Ty) -> Ty {
        let int = match self.lane_bits(elem) {
            Some(8) => IntTy::I8,
            Some(16) => IntTy::I16,
            Some(32) => IntTy::I32,
            _ => IntTy::I64,
        };
        let lane = self.cx.types.int(int);
        self.vector_of(lane)
    }

    /// `arg` checked as a vector, of type `wanted` when that is known;
    /// None after reporting what it is instead.
    fn simd_vector(&mut self, name: &str, arg: &Expression, wanted: Option<Ty>) -> Option<Expr> {
        let value = self.value(arg, wanted);
        if self.cx.types.is_error(value.ty) {
            return None;
        }
        if self.lanes_of(value.ty).is_some() && wanted.is_none_or(|ty| ty == value.ty) {
            return Some(value);
        }
        let shown = self.ty(value.ty);
        let message = match wanted {
            Some(ty) => {
                let want = self.ty(ty);
                format!("'simd::{name}' takes vectors of one type, {want}; this is {shown}")
            }
            None => format!(
                "'simd::{name}' takes vectors, arrays of {} bytes as Array<i32, simd::LANES_32>; this is {shown}",
                self.cx.simd_bytes
            ),
        };
        self.error(message, arg.span);
        None
    }

    /// Two vectors of one type, the one not written as an array literal
    /// checked first so that the literal takes its type.
    fn simd_pair(&mut self, name: &str, a: &Expression, b: &Expression) -> Option<(Expr, Expr)> {
        let literal = |e: &Expression| {
            matches!(
                e.kind,
                ExpressionKind::ArrayLiteral(_) | ExpressionKind::ArrayRepeat { .. }
            )
        };
        if literal(a) && !literal(b) {
            let second = self.simd_vector(name, b, None)?;
            let first = self.simd_vector(name, a, Some(second.ty))?;
            return Some((first, second));
        }
        let first = self.simd_vector(name, a, None)?;
        let second = self.simd_vector(name, b, Some(first.ty))?;
        Some((first, second))
    }

    pub(super) fn simd_call(
        &mut self,
        op: SimdOp,
        name: &str,
        args: &[Expression],
        span: Span,
    ) -> Expr {
        let wanted = match op {
            SimdOp::Load | SimdOp::Sum => 1,
            SimdOp::Select => 3,
            _ => 2,
        };
        if args.len() != wanted {
            let given = args.len();
            self.error(
                format!("'simd::{name}' takes {wanted} argument(s), but {given} were given"),
                span,
            );
            self.check_args_only(args);
            return self.error_expr(span);
        }
        match self.simd_checked(op, name, args, span) {
            Some((values, ty)) => self.mk(ExprKind::Builtin(Builtin::Simd(op), values), ty, span),
            None => self.error_expr(span),
        }
    }

    /// The arguments of a std.simd call and the type it gives, or None
    /// after reporting what is wrong.
    fn simd_checked(
        &mut self,
        op: SimdOp,
        name: &str,
        args: &[Expression],
        span: Span,
    ) -> Option<(Vec<Expr>, Ty)> {
        match op {
            SimdOp::Load => {
                let slice = self.value(&args[0], None);
                let elem = match *self.cx.types.kind(slice.ty) {
                    TyKind::Slice(elem) | TyKind::SliceMut(elem)
                        if self.lane_bits(elem).is_some() =>
                    {
                        elem
                    }
                    _ if self.cx.types.is_error(slice.ty) => return None,
                    _ => {
                        let shown = self.ty(slice.ty);
                        self.error(
                            format!("'simd::load' takes a slice of numbers, as values[i..]; this is {shown}"),
                            args[0].span,
                        );
                        return None;
                    }
                };
                let ty = self.vector_of(elem);
                let TyKind::Array(_, lanes) = *self.cx.types.kind(ty) else {
                    unreachable!("a vector is an array")
                };
                let lanes = self.mk(ExprKind::Int(lanes as i128), Types::USIZE, span);
                Some((vec![slice, lanes], ty))
            }
            SimdOp::Store => {
                let out = self.value(&args[0], None);
                let elem = match *self.cx.types.kind(out.ty) {
                    TyKind::SliceMut(elem) if self.lane_bits(elem).is_some() => elem,
                    _ if self.cx.types.is_error(out.ty) => return None,
                    _ => {
                        let shown = self.ty(out.ty);
                        self.error(
                            format!("'simd::store' writes into a slice that writes, as &var out[i..]; this is {shown}"),
                            args[0].span,
                        );
                        return None;
                    }
                };
                let vector = self.vector_of(elem);
                let lanes = self.simd_vector(name, &args[1], Some(vector))?;
                Some((vec![out, lanes], Types::VOID))
            }
            SimdOp::Sum => {
                let lanes = self.simd_vector(name, &args[0], None)?;
                let elem = self.lanes_of(lanes.ty).expect("a vector");
                Some((vec![lanes], elem))
            }
            SimdOp::Shl | SimdOp::Shr => {
                let lanes = self.simd_vector(name, &args[0], None)?;
                self.integer_lanes(name, &lanes)?;
                let amount = self.value(&args[1], None);
                if self.cx.types.as_int(amount.ty).is_none() {
                    if !self.cx.types.is_error(amount.ty) {
                        let shown = self.ty(amount.ty);
                        self.error(
                            format!("'simd::{name}' shifts by an integer, not {shown}"),
                            args[1].span,
                        );
                    }
                    return None;
                }
                let ty = lanes.ty;
                Some((vec![lanes, amount], ty))
            }
            SimdOp::Select => {
                let mask = self.simd_vector(name, &args[0], None)?;
                let (a, b) = self.simd_pair(name, &args[1], &args[2])?;
                let elem = self.lanes_of(a.ty).expect("a vector");
                let wanted = self.mask_of(elem);
                if mask.ty != wanted {
                    let (want, got) = (self.ty(wanted), self.ty(mask.ty));
                    self.error(
                        format!("The mask of 'simd::select' is a vector of integers as wide as the lanes, {want}; this is {got}"),
                        args[0].span,
                    );
                    return None;
                }
                let ty = a.ty;
                Some((vec![mask, a, b], ty))
            }
            _ => {
                let (a, b) = self.simd_pair(name, &args[0], &args[1])?;
                let elem = self.lanes_of(a.ty).expect("a vector");
                let ty = match op {
                    SimdOp::And | SimdOp::Or | SimdOp::Xor => {
                        self.integer_lanes(name, &a)?;
                        a.ty
                    }
                    SimdOp::Eq | SimdOp::Lt | SimdOp::Gt => self.mask_of(elem),
                    SimdOp::Madd => {
                        if self.cx.types.as_int(elem) != Some(IntTy::I16) {
                            let shown = self.ty(a.ty);
                            self.error(
                                format!("'simd::madd' takes vectors of i16, not {shown}"),
                                args[0].span,
                            );
                            return None;
                        }
                        self.vector_of(Types::I32)
                    }
                    _ => a.ty,
                };
                Some((vec![a, b], ty))
            }
        }
    }

    /// Report a vector of floats where only integer lanes go.
    fn integer_lanes(&mut self, name: &str, lanes: &Expr) -> Option<()> {
        let elem = self.lanes_of(lanes.ty).expect("a vector");
        if self.cx.types.as_int(elem).is_some() {
            return Some(());
        }
        let shown = self.ty(elem);
        self.error(
            format!("'simd::{name}' works on integer lanes, not {shown}"),
            lanes.span,
        );
        None
    }
}
