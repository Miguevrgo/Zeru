//! Global constants, and evaluating integer expressions at compile time at
//! the width of their type, as the program would at runtime.

use crate::ast::{Expression, ExpressionKind, StatementKind};
use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{BinOp, Expr, ExprKind, UnOp};
use crate::ty::{IntTy, Types};

use super::body::FnCx;
use super::{Checker, ConstState};

impl Checker<'_> {
    /// The value of the constant `def`, checked the first time it is needed,
    /// at the type it declares.
    pub(super) fn const_value(&mut self, def: DefId, used: Span) -> Expr {
        self.note_use(def, used);
        match self.consts.get(&def) {
            Some(ConstState::Done(value)) => return value.clone(),
            Some(ConstState::Checking) => {
                let name = self.items.item(def).name.clone();
                self.error(format!("'{name}' is defined in terms of itself"), used);
                return Expr {
                    kind: ExprKind::Void,
                    ty: Types::ERROR,
                    span: used,
                };
            }
            None => {}
        }
        self.consts.insert(def, ConstState::Checking);
        let item = self.items.item(def);
        let StatementKind::Var {
            name,
            value,
            type_annotation,
            ..
        } = &item.decl.kind
        else {
            unreachable!("a constant is a declaration")
        };
        let span = item.decl.span;
        let scope = self.scope_of(def, &[]);
        let expected = type_annotation
            .as_ref()
            .map(|spec| self.resolve_type(spec, &scope, span));
        let errors = self.errors.len();
        let mut cx = FnCx::for_constant(self, scope);
        let checked = cx.expr(value, expected);
        let mut checked = match expected {
            Some(ty) => cx.coerce_or(checked, ty, span, |want, got| {
                format!("Type mismatch for variable '{name}'. Annotated as {want} but got {got}")
            }),
            None => checked,
        };
        let views = cx.views();
        if !is_constant(value) && self.errors.len() == errors {
            checked = match self.run_constant(&checked, &views, name, span) {
                Some(value) => value,
                None => Expr {
                    kind: ExprKind::Void,
                    ty: Types::ERROR,
                    span,
                },
            };
        }
        self.consts.insert(def, ConstState::Done(checked.clone()));
        checked
    }

    /// The length a constant integer expression gives an array; None
    /// after reporting one that is not constant or does not fit.
    pub(super) fn length(&mut self, expr: &Expr) -> Option<u64> {
        match self.types.as_int(expr.ty).and(self.eval(expr)) {
            Some(value) => self.fitting_length(value, expr.span),
            None => {
                if expr.ty != Types::ERROR {
                    self.error("An array length must be a constant integer", expr.span);
                }
                None
            }
        }
    }

    /// `value` as an array length, which LLVM holds in 32 bits; None after
    /// reporting one outside that.
    pub(super) fn fitting_length(&mut self, value: i128, span: Span) -> Option<u64> {
        let len = u32::try_from(value).ok().map(u64::from);
        if len.is_none() {
            self.error(
                format!("An array length goes from 0 to {}, not {value}", u32::MAX),
                span,
            );
        }
        len
    }

    /// The integer an expression always has, if it is made of constants and
    /// stays within its type; a bool counts as 0 or 1.
    pub(super) fn eval(&self, expr: &Expr) -> Option<i128> {
        let int = self.types.as_int(expr.ty);
        let value = match &expr.kind {
            ExprKind::Int(value) => *value,
            ExprKind::Bool(value) => i128::from(*value),
            ExprKind::Unary(UnOp::Neg, operand) => self.eval(operand)?.checked_neg()?,
            ExprKind::Unary(UnOp::Not, operand) if expr.ty == Types::BOOL => {
                1 - self.eval(operand)?
            }
            ExprKind::Unary(UnOp::Not, operand) => wrap(!self.eval(operand)?, int?),
            ExprKind::Binary(op, left, right) => {
                arith(*op, self.eval(left)?, self.eval(right)?, int)?
            }
            ExprKind::Cast(operand) if self.types.as_int(operand.ty).is_some() => {
                wrap(self.eval(operand)?, int?)
            }
            _ => return None,
        };
        int.is_none_or(|int| int.contains(value)).then_some(value)
    }
}

/// `value` as an integer of type `int` holds it: its low bits, read with
/// that type's sign.
pub(super) fn wrap(value: i128, int: IntTy) -> i128 {
    let bits = int.bits();
    let low = value & ((1 << bits) - 1);
    if int.signed() && low >> (bits - 1) == 1 {
        low - (1 << bits)
    } else {
        low
    }
}

/// `l op r` on integers of type `int`, or on bools without one; none when it
/// overflows, divides by zero, shifts by the width or more, or is no such
/// operation.
pub(super) fn arith(op: BinOp, l: i128, r: i128, int: Option<IntTy>) -> Option<i128> {
    let shift = || int.filter(|int| (0..i128::from(int.bits())).contains(&r));
    Some(match op {
        BinOp::Add => l.checked_add(r)?,
        BinOp::Sub => l.checked_sub(r)?,
        BinOp::Mul => l.checked_mul(r)?,
        BinOp::Div => l.checked_div(r)?,
        BinOp::Rem => l.checked_rem(r)?,
        BinOp::AddWrap => wrap(l.wrapping_add(r), int?),
        BinOp::SubWrap => wrap(l.wrapping_sub(r), int?),
        BinOp::MulWrap => wrap(l.wrapping_mul(r), int?),
        BinOp::BitAnd => l & r,
        BinOp::BitOr => l | r,
        BinOp::BitXor => l ^ r,
        BinOp::Shl => wrap(l << r, shift()?),
        BinOp::Shr => shift().map(|_| l >> r)?,
        _ => return None,
    })
}

impl FnCx<'_, '_> {
    /// Report arithmetic on constants whose result its type cannot hold, a
    /// constant division by zero, and a constant shift by at least the width.
    pub(super) fn check_constant(&mut self, expr: &Expr) {
        let Some(int) = self.cx.types.as_int(expr.ty) else {
            return;
        };
        let shown = self.ty(expr.ty);
        match &expr.kind {
            ExprKind::Binary(BinOp::Shl | BinOp::Shr, _, right) => {
                if let Some(amount) = self.cx.eval(right)
                    && !(0..i128::from(int.bits())).contains(&amount)
                {
                    self.error(
                        format!("Shift by {amount} is out of range for {shown}"),
                        expr.span,
                    );
                }
            }
            ExprKind::Binary(
                op @ (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem),
                left,
                right,
            ) => {
                let (Some(l), Some(r)) = (self.cx.eval(left), self.cx.eval(right)) else {
                    return;
                };
                let result = match op {
                    BinOp::Div | BinOp::Rem if r == 0 => {
                        self.error("Division by zero in a constant expression", expr.span);
                        return;
                    }
                    BinOp::Rem if !int.contains(l / r) => None,
                    _ => arith(*op, l, r, Some(int)),
                };
                self.report_overflow(result, int, &shown, expr.span);
            }
            ExprKind::Unary(UnOp::Neg, operand) => {
                if let Some(value) = self.cx.eval(operand) {
                    self.report_overflow(value.checked_neg(), int, &shown, expr.span);
                }
            }
            _ => {}
        }
    }

    fn report_overflow(&mut self, result: Option<i128>, int: IntTy, shown: &str, span: Span) {
        match result {
            Some(value) if !int.contains(value) => {
                self.error(format!("Constant {value} does not fit in {shown}"), span);
            }
            None => self.error(format!("Constant expression overflows {shown}"), span),
            Some(_) => {}
        }
    }
}

/// Whether a global constant is written with literals, other constants and
/// operators on them alone, which need no running to give its value.
fn is_constant(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Int(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::Boolean(_)
        | ExpressionKind::StringLit(_)
        | ExpressionKind::Identifier(_) => true,
        ExpressionKind::Prefix { right: inner, .. }
        | ExpressionKind::Cast { left: inner, .. }
        | ExpressionKind::ArrayRepeat { value: inner, .. } => is_constant(inner),
        ExpressionKind::Infix { left, right, .. } => is_constant(left) && is_constant(right),
        ExpressionKind::ArrayLiteral(items) | ExpressionKind::Tuple(items) => {
            items.iter().all(is_constant)
        }
        ExpressionKind::Get { object, name } => {
            matches!(object.kind, ExpressionKind::Identifier(_))
                && matches!(name.as_str(), "MAX" | "MIN")
        }
        _ => false,
    }
}
