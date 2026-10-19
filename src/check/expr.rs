//! Expressions: literals, names, operators, casts, literals of structs,
//! arrays and tuples, places, assignment and `?`/`!`.

use std::collections::HashMap;

use crate::ast::StatementKind;
use crate::ast::{Expression, ExpressionKind, TypeSpec};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::tir::{Asm, BinOp, Expr, ExprKind, Place, UnOp};
use crate::token::Token;
use crate::ty::{Ty, TyKind, Types};

use super::Checker;
use super::body::FnCx;

impl FnCx<'_, '_> {
    pub(super) fn mk(&self, kind: ExprKind, ty: Ty, span: Span) -> Expr {
        Expr { kind, ty, span }
    }

    /// The expression standing for one that failed to check. Its error must
    /// have been reported; one that was not is remembered as a compiler bug.
    pub(super) fn error_expr(&self, span: Span) -> Expr {
        if self.cx.errors.is_empty() && self.cx.unreported.get().is_none() {
            self.cx.unreported.set(Some(span));
        }
        self.mk(ExprKind::Void, Types::ERROR, span)
    }

    pub(super) fn expr(&mut self, e: &Expression, expected: Option<Ty>) -> Expr {
        let expected = expected.map(|ty| self.resolve(ty));
        let errors = self.cx.errors.len();
        let expr = self.expr_inner(e, expected);
        if self.cx.errors.len() == errors {
            self.check_constant(&expr);
        }
        expr
    }

    /// `e` where its value is used, which something returning nothing has
    /// not, as in C.
    pub(super) fn value(&mut self, e: &Expression, expected: Option<Ty>) -> Expr {
        let value = self.expr(e, expected);
        if value.ty != Types::VOID {
            return value;
        }
        let message = match &e.kind {
            ExpressionKind::Call { function, .. } => match &function.kind {
                ExpressionKind::Identifier(name) | ExpressionKind::Get { name, .. } => {
                    format!("{name}() returns nothing, so it has no value to use")
                }
                _ => "This call returns nothing, so it has no value to use".into(),
            },
            _ => "This has no value to use".into(),
        };
        self.error(message, e.span);
        self.error_expr(e.span)
    }

    /// The expected type a literal takes its own from: a `T?` hands down `T`.
    fn unwrap_expected(&self, expected: Option<Ty>) -> Option<Ty> {
        match expected.map(|ty| self.cx.types.kind(ty)) {
            Some(TyKind::Optional(inner) | TyKind::Result(inner, _)) => Some(*inner),
            _ => expected,
        }
    }

    fn expr_inner(&mut self, e: &Expression, expected: Option<Ty>) -> Expr {
        let span = e.span;
        let inner = self.unwrap_expected(expected);
        match &e.kind {
            ExpressionKind::Int(value) => self.int_literal(*value, false, inner, span),
            ExpressionKind::Float(value) => self.float_literal(*value, inner, span),
            ExpressionKind::Boolean(value) => self.mk(ExprKind::Bool(*value), Types::BOOL, span),
            ExpressionKind::StringLit(bytes) => {
                self.mk(ExprKind::Str(bytes.clone()), Types::STR, span)
            }
            ExpressionKind::None => match expected.map(|ty| (ty, self.cx.types.kind(ty))) {
                Some((ty, TyKind::Optional(_))) => self.mk(ExprKind::None, ty, span),
                Some((ty, _)) if self.cx.types.is_error(ty) => self.error_expr(span),
                Some((ty, _)) if !self.cx.types.contains_var(ty) => {
                    let shown = self.ty(ty);
                    self.error(
                        format!("'None' can only be assigned to optional types, got {shown}"),
                        span,
                    );
                    self.error_expr(span)
                }
                _ => {
                    self.error("Cannot tell the type of None here; annotate it", span);
                    self.error_expr(span)
                }
            },
            ExpressionKind::Identifier(name) => self.identifier(name, span),
            ExpressionKind::Get { .. }
            | ExpressionKind::Index { .. }
            | ExpressionKind::Dereference(_) => {
                let (place, ty) = self.place(e);
                if self.cx.types.is_error(ty) {
                    return self.error_expr(span);
                }
                self.mk(ExprKind::Read(place), ty, span)
            }
            ExpressionKind::Assign {
                target,
                operator,
                value,
            } => self.assign(target, operator, value, span),
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => self.infix(left, operator, right, expected, span),
            ExpressionKind::Prefix { operator, right } => {
                self.prefix(operator, right, expected, span)
            }
            ExpressionKind::Try { operator, value } => self.try_expr(operator, value, span),
            ExpressionKind::Block(statements) => {
                let block = self.block(statements);
                self.mk(ExprKind::Block(block), Types::VOID, span)
            }
            ExpressionKind::Call {
                function,
                type_args,
                arguments,
            } => self.call(function, type_args, arguments, expected, span),
            ExpressionKind::StructLiteral { name, fields } => {
                self.struct_literal(name, fields, inner, span)
            }
            ExpressionKind::Match { value, arms } => self.match_expr(value, arms, expected, span),
            ExpressionKind::Cast { left, target } => self.cast(left, target, span),
            ExpressionKind::ArrayLiteral(elements) => self.array_literal(elements, inner, span),
            ExpressionKind::ArrayRepeat { value, count } => {
                self.array_repeat(value, *count, inner, span)
            }
            ExpressionKind::Range { .. } => {
                self.error("A range only goes in a 'for' loop", span);
                self.error_expr(span)
            }
            ExpressionKind::Tuple(elements) => self.tuple(elements, inner, span),
            ExpressionKind::BorrowRef(place) => {
                let mutable = matches!(
                    expected.map(|t| self.cx.types.kind(t)),
                    Some(TyKind::Ptr(_))
                );
                self.borrow(place, mutable, span)
            }
            ExpressionKind::BorrowRefMut(place) => self.borrow(place, true, span),
            ExpressionKind::InlineAsm {
                template,
                outputs,
                inputs,
                clobbers,
                is_volatile,
            } => {
                let values: Vec<Expr> = inputs.iter().map(|op| self.expr(&op.expr, None)).collect();
                let mut places = Vec::new();
                for output in outputs {
                    if !matches!(output.expr.kind, ExpressionKind::Identifier(_)) {
                        self.error(
                            "Inline assembly output must be a variable",
                            output.expr.span,
                        );
                    }
                    places.push(self.place(&output.expr).0);
                }
                let constraints: Vec<String> = outputs
                    .iter()
                    .chain(inputs)
                    .map(|op| op.constraint.clone())
                    .chain(clobbers.iter().map(|c| format!("~{{{c}}}")))
                    .collect();
                let kind = ExprKind::Asm(Box::new(Asm {
                    template: template.clone(),
                    constraints: constraints.join(","),
                    outputs: places,
                    inputs: values,
                    volatile: *is_volatile,
                }));
                self.mk(kind, Types::I64, span)
            }
        }
    }

    /// An integer written out, negated when `negative`: of the integer type
    /// its context expects, or i32 when nothing does. A value that does not
    /// fit is an error, not a quiet wrap.
    pub(super) fn float_literal(&mut self, value: f64, expected: Option<Ty>, span: Span) -> Expr {
        if let Some(var) = self.literal_var(expected, Types::F64) {
            return self.mk(ExprKind::Float(value), var, span);
        }
        match expected.map(|t| self.cx.types.kind(t)) {
            Some(TyKind::F32) => {
                if (value as f32).is_infinite() {
                    self.error(format!("Literal {value:e} does not fit in f32"), span);
                }
                self.mk(ExprKind::Float(value), Types::F32, span)
            }
            _ => self.mk(ExprKind::Float(value), Types::F64, span),
        }
    }

    pub(super) fn int_literal(
        &mut self,
        value: u64,
        negative: bool,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let signed = if negative {
            -i128::from(value)
        } else {
            i128::from(value)
        };
        if let Some(var) = self.literal_var(expected, Types::I32) {
            return self.mk(ExprKind::Int(signed), var, span);
        }
        if let Some(ty) = expected.filter(|&t| self.cx.types.is_float(t)) {
            let exact = match self.cx.types.kind(ty) {
                TyKind::F32 => (value as f32) as u128 == u128::from(value),
                _ => (value as f64) as u128 == u128::from(value),
            };
            if !exact {
                let shown = self.ty(ty);
                self.error(format!("Literal {value} has no exact {shown} value"), span);
            }
            return self.mk(ExprKind::Float(signed as f64), ty, span);
        }
        let (ty, by_default) = match expected.and_then(|t| self.cx.types.as_int(t).map(|_| t)) {
            Some(ty) => (ty, false),
            None => (Types::I32, true),
        };
        let int = self.cx.types.as_int(ty).unwrap();
        if !int.contains(signed) {
            let sign = if negative { "-" } else { "" };
            let hint = if by_default {
                ", the type a literal takes when nothing says otherwise; annotate a wider type"
            } else {
                ""
            };
            let shown = self.ty(ty);
            self.error(
                format!("Literal {sign}{value} does not fit in {shown}{hint}"),
                span,
            );
        }
        self.mk(ExprKind::Int(signed), ty, span)
    }

    fn identifier(&mut self, name: &str, span: Span) -> Expr {
        if let Some(local) = self.local(name) {
            let ty = self.local_ty(local);
            return self.mk(ExprKind::Read(Place::Local(local)), ty, span);
        }
        if let Some((owner, variant)) = name.rsplit_once("::")
            && let Some(def) = self.cx.items.lookup_type(self.scope.module, owner)
            && self.cx.items.item(def).kind == ItemKind::Enum
        {
            self.cx.check_visible(def, self.scope.module, span);
            let variants = self.cx.variants_of(def);
            let Some(index) = variants.iter().position(|(v, _)| v == variant) else {
                self.error(format!("Enum '{owner}' has no variant '{variant}'"), span);
                return self.error_expr(span);
            };
            if !variants[index].1.is_empty() {
                let count = variants[index].1.len();
                self.error(
                    format!("'{name}' carries {count} value(s): write {name}(...)"),
                    span,
                );
                return self.error_expr(span);
            }
            let ty = self.cx.types.intern(TyKind::Adt(def, Vec::new()));
            return self.mk(ExprKind::Variant(index as u32, Vec::new()), ty, span);
        }
        match self.cx.items.lookup_value(self.scope.module, name) {
            Some(def) if self.cx.items.item(def).kind == ItemKind::Const => {
                self.cx.check_visible(def, self.scope.module, span);
                let mut value = self.cx.const_value(def, span);
                value.span = span;
                value
            }
            Some(_) => {
                self.error(format!("'{name}' is a function, not a variable"), span);
                self.error_expr(span)
            }
            None => {
                let mut candidates = self.local_names();
                candidates.extend(
                    self.cx
                        .items
                        .items
                        .iter()
                        .filter(|item| item.kind == ItemKind::Const)
                        .map(|item| item.name.clone()),
                );
                match super::types::closest(name, &candidates) {
                    Some(similar) => self.error(
                        format!("Undeclared variable '{name}'. Did you mean '{similar}'?"),
                        span,
                    ),
                    None => self.error(format!("Undeclared variable '{name}'"), span),
                }
                self.error_expr(span)
            }
        }
    }

    /// Where `e` lives: a local, a field, an element, what a pointer points
    /// at, or, for a value with no place of its own, a temporary.
    pub(super) fn place(&mut self, e: &Expression) -> (Place, Ty) {
        let span = e.span;
        match &e.kind {
            ExpressionKind::Identifier(name) if self.local(name).is_some() => {
                let value = self.identifier(name, span);
                match value.kind {
                    ExprKind::Read(place) => (place, value.ty),
                    _ => (Place::Temp(Box::new(value)), Types::ERROR),
                }
            }
            ExpressionKind::Get { object, name } => self.field(object, name, span),
            ExpressionKind::Index { left, index } => self.index(left, index, span),
            ExpressionKind::Dereference(pointer) => {
                let pointer = self.expr(pointer, None);
                let pointee = match self.cx.types.kind(pointer.ty) {
                    TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                    TyKind::Error => Types::ERROR,
                    _ => {
                        let shown = self.ty(pointer.ty);
                        self.error(format!("Cannot dereference type {shown}"), span);
                        Types::ERROR
                    }
                };
                (Place::Deref(Box::new(pointer)), pointee)
            }
            _ => {
                let value = self.expr(e, None);
                let ty = value.ty;
                (Place::Temp(Box::new(value)), ty)
            }
        }
    }

    /// `object.name`: a struct's field or a tuple's part, through one pointer
    /// or reference.
    fn field(&mut self, object: &Expression, name: &str, span: Span) -> (Place, Ty) {
        let (base, ty) = self.place(object);
        let (base, ty) = match *self.cx.types.kind(ty) {
            TyKind::Ptr(_) => self.deref(base, ty, object.span),
            _ => self.through_ref(base, ty, object.span),
        };
        match self.cx.types.kind(ty).clone() {
            TyKind::Error => (base, Types::ERROR),
            TyKind::Tuple(parts) => match name.parse::<usize>().ok().filter(|&at| at < parts.len())
            {
                Some(at) => (Place::Field(Box::new(base), at as u32), parts[at]),
                None => {
                    let shown = self.ty(ty);
                    self.error(format!("{shown} has no field '{name}'"), span);
                    (base, Types::ERROR)
                }
            },
            TyKind::Adt(..) if self.cx.is_struct(ty).is_some() => {
                let fields = self.cx.fields_of(ty);
                match fields.iter().position(|(field, _)| field == name) {
                    Some(at) => (Place::Field(Box::new(base), at as u32), fields[at].1),
                    None => {
                        let shown = self.ty(ty);
                        self.error(format!("Struct '{shown}' has no field '{name}'"), span);
                        (base, Types::ERROR)
                    }
                }
            }
            _ => {
                self.error("Cannot access property on non-struct type", span);
                (base, Types::ERROR)
            }
        }
    }

    fn index(&mut self, left: &Expression, index: &Expression, span: Span) -> (Place, Ty) {
        let (base, base_ty) = self.place(left);
        let (base, base_ty) = self.through_ref(base, base_ty, left.span);
        let index_expr = self.expr(index, Some(Types::USIZE));
        if self.cx.types.as_int(index_expr.ty).is_none() && !self.cx.types.is_error(index_expr.ty) {
            let shown = self.ty(index_expr.ty);
            self.error(format!("Index must be an integer, got {shown}"), index.span);
        }
        let elem = match self.cx.types.kind(base_ty).clone() {
            TyKind::Array(elem, len) => {
                if let ExprKind::Int(at) = index_expr.kind
                    && at >= i128::from(len)
                {
                    self.error(
                        format!("Index {at} is outside the array's 0..{len}"),
                        index.span,
                    );
                }
                elem
            }
            TyKind::Vec(elem) | TyKind::Slice(elem) => elem,
            TyKind::Error => Types::ERROR,
            TyKind::Ptr(pointee) if matches!(self.cx.types.kind(pointee), TyKind::Array(..)) => {
                let shown = self.ty(base_ty);
                self.error(
                    format!("Cannot index {shown} directly, write (*p)[i]"),
                    span,
                );
                Types::ERROR
            }
            _ => {
                let shown = self.ty(base_ty);
                self.error(format!("Cannot index type {shown}"), span);
                Types::ERROR
            }
        };
        (Place::Index(Box::new(base), Box::new(index_expr)), elem)
    }

    /// What the pointer of type `ty` read from `place` points at.
    pub(super) fn deref(&mut self, place: Place, ty: Ty, span: Span) -> (Place, Ty) {
        let pointee = match *self.cx.types.kind(ty) {
            TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => inner,
            _ => unreachable!("only a pointer is followed"),
        };
        let pointer = self.mk(ExprKind::Read(place), ty, span);
        (Place::Deref(Box::new(pointer)), pointee)
    }

    /// `place` itself, or what it refers to when it holds a reference: a
    /// field, an element or a method is reached through a `&T` as in Rust.
    pub(super) fn through_ref(&mut self, place: Place, ty: Ty, span: Span) -> (Place, Ty) {
        match self.cx.types.kind(ty) {
            TyKind::Ref(_) | TyKind::RefMut(_) => self.deref(place, ty, span),
            _ => (place, ty),
        }
    }

    fn borrow(&mut self, inner: &Expression, mutable: bool, span: Span) -> Expr {
        let (place, ty) = self.place(inner);
        if mutable {
            self.require_mutable(&place, inner, span);
        }
        if self.temporary(&place, ty, inner) {
            self.error("Cannot create reference to a temporary value", span);
        }
        let kind = if mutable {
            TyKind::RefMut(ty)
        } else {
            TyKind::Ref(ty)
        };
        let ty = self.cx.intern_or_error(kind, &[ty]);
        self.note_borrow(&place);
        self.mk(ExprKind::AddressOf(place), ty, span)
    }

    fn assign(
        &mut self,
        target: &Expression,
        operator: &Token,
        value: &Expression,
        span: Span,
    ) -> Expr {
        let revived = match &target.kind {
            ExpressionKind::Identifier(name) => self.local(name),
            _ => None,
        };
        let (place, target_ty) = match revived {
            Some(local) => (Place::Local(local), self.local_ty(local)),
            None => self.place(target),
        };
        if self.temporary(&place, target_ty, target) {
            self.error("Cannot assign to a temporary value", target.span);
        }
        match revived {
            Some(local) if !self.local_mutable(local) => {
                let name = self.local_name(local).to_string();
                self.error(format!("Cannot reassign constant variable '{name}'"), span);
            }
            Some(_) => {}
            None => self.require_mutable(&place, target, span),
        }
        if let Place::Index(base, _) = &place
            && self
                .place_ty(base)
                .is_some_and(|ty| matches!(self.cx.types.kind(ty), TyKind::Slice(_)))
        {
            self.error(
                "Cannot write through a slice, which may point at read-only memory",
                span,
            );
            return self.mk(ExprKind::Void, Types::VOID, span);
        }

        let checked = self.expr(value, Some(target_ty));
        let compound = operator.compound_base();
        let mut value = match self.coerce(checked, target_ty) {
            Ok(value) => {
                if let Some(base) = &compound
                    && !self.cx.types.is_error(target_ty)
                    && !self.cx.operator_applies(base, target_ty)
                {
                    let shown = self.ty(target_ty);
                    self.error(
                        format!("Operator '{operator}' cannot be applied to {shown}"),
                        span,
                    );
                }
                value
            }
            Err(value) => {
                let (want, got) = (self.ty(target_ty), self.ty(value.ty));
                self.error(
                    format!("Type mismatch in assignment. Expected {want}, got {got}"),
                    span,
                );
                value
            }
        };
        self.consume(&mut value);
        let kind = match compound {
            Some(base) => ExprKind::CompoundAssign {
                op: binop(&base),
                place,
                value: Box::new(value),
            },
            None => ExprKind::Assign {
                drop_old: self.cx.owns_heap(target_ty) && !self.writes_raw(&place),
                place,
                value: Box::new(value),
            },
        };
        self.mk(kind, Types::VOID, span)
    }

    /// Whether `place` is what a raw `*T` points at, which may hold no value
    /// yet: writing there drops nothing, as Rust's `ptr::write`.
    fn writes_raw(&self, place: &Place) -> bool {
        matches!(place, Place::Deref(pointer) if matches!(self.cx.types.kind(pointer.ty), TyKind::Ptr(_)))
    }

    /// The constant whose value `place`, written as `ast`, is part of. A
    /// constant is a value and not a place, as in rustc, so a write to it
    /// would change only a copy.
    pub(super) fn constant_written(&self, place: &Place, ast: &Expression) -> Option<String> {
        if !super::body::in_temporary(place) {
            return None;
        }
        let mut root = ast;
        while let ExpressionKind::Get { object, .. } | ExpressionKind::Index { left: object, .. } =
            &root.kind
        {
            root = object;
        }
        let ExpressionKind::Identifier(name) = &root.kind else {
            return None;
        };
        let def = self.cx.items.lookup_value(self.scope.module, name)?;
        (self.local(name).is_none() && self.cx.items.item(def).kind == ItemKind::Const)
            .then(|| name.clone())
    }

    /// Whether `place` of type `ty`, written as `ast`, is a temporary value
    /// that is not a constant's.
    pub(super) fn temporary(&self, place: &Place, ty: Ty, ast: &Expression) -> bool {
        super::body::in_temporary(place)
            && !self.cx.types.is_error(ty)
            && self.constant_written(place, ast).is_none()
    }

    /// The type of what `place` holds.
    pub(super) fn place_ty(&mut self, place: &Place) -> Option<Ty> {
        Some(match place {
            Place::Local(local) => self.local_ty(*local),
            Place::Field(base, at) => {
                let base = self.place_ty(base)?;
                match self.cx.types.kind(base).clone() {
                    TyKind::Tuple(parts) => *parts.get(*at as usize)?,
                    _ => self.cx.fields_of(base).get(*at as usize)?.1,
                }
            }
            Place::Index(base, _) => {
                let base = self.place_ty(base)?;
                match self.cx.types.kind(base) {
                    TyKind::Array(elem, _) | TyKind::Vec(elem) | TyKind::Slice(elem) => *elem,
                    _ => return None,
                }
            }
            Place::Deref(pointer) => match self.cx.types.kind(pointer.ty) {
                TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                _ => return None,
            },
            Place::Temp(value) => value.ty,
        })
    }

    /// Type two operands. A number written out takes the type of the other
    /// one, on either side: `3 < x` compares at the type of `x`, as `x > 3`.
    pub(super) fn operands(
        &mut self,
        left: &Expression,
        right: &Expression,
        expected: Option<Ty>,
    ) -> (Expr, Expr) {
        if is_number_literal(left) && !is_number_literal(right) {
            let right = self.expr(right, expected);
            let left = self.expr(left, Some(right.ty));
            (left, right)
        } else {
            let left = self.expr(left, expected);
            let right = self.expr(right, Some(left.ty));
            (left, right)
        }
    }

    fn infix(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let comparison = matches!(
            operator,
            Token::Eq | Token::NotEq | Token::Lt | Token::Gt | Token::Leq | Token::Geq
        );
        let hint = expected.filter(|&ty| !comparison && !self.cx.types.contains_var(ty));
        let (l, r) = self.operands(left, right, hint);
        let types = &self.cx.types;
        if types.is_error(l.ty) || types.is_error(r.ty) {
            return self.error_expr(span);
        }
        if matches!(operator, Token::And | Token::Or)
            && let Some(other) = [l.ty, r.ty].into_iter().find(|&ty| ty != Types::BOOL)
        {
            let shown = self.ty(other);
            self.error(
                format!("Operator '{operator}' cannot be applied to {shown}"),
                span,
            );
            return self.error_expr(span);
        }
        if let TyKind::Ptr(_) = types.kind(l.ty)
            && matches!(operator, Token::Plus | Token::Minus)
            && types.as_int(r.ty).is_some()
        {
            let op = if *operator == Token::Plus {
                BinOp::PtrAdd
            } else {
                BinOp::PtrSub
            };
            let ty = l.ty;
            return self.mk(ExprKind::Binary(op, Box::new(l), Box::new(r)), ty, span);
        }
        let (l_ty, r) = match self.coerce(r, l.ty) {
            Ok(r) if !matches!(r.kind, ExprKind::Some(_) | ExprKind::Ok(_)) => (l.ty, r),
            Ok(r) | Err(r) => {
                let (lt, rt) = (self.ty(l.ty), self.ty(r.ty));
                self.error(
                    format!("Binary operation '{operator}' requires operands of same type. Got {lt} and {rt}"),
                    span,
                );
                return self.error_expr(span);
            }
        };
        if let Some(def) = self.cx.is_enum(l_ty)
            && self.cx.enum_has_data(def)
        {
            let shown = self.ty(l_ty);
            self.error(
                format!("'{operator}' does not apply to {shown}, whose variants carry values; use a match"),
                span,
            );
            return self.error_expr(span);
        }
        if !self.cx.operator_applies(operator, l_ty) {
            let shown = self.ty(l_ty);
            self.error(
                format!("Operator '{operator}' cannot be applied to {shown}"),
                span,
            );
            return self.error_expr(span);
        }
        match operator {
            Token::And => self.mk(ExprKind::And(Box::new(l), Box::new(r)), Types::BOOL, span),
            Token::Or => self.mk(ExprKind::Or(Box::new(l), Box::new(r)), Types::BOOL, span),
            Token::Eq | Token::NotEq if l_ty == Types::STR => {
                let eq = self.cx.prelude_fn("__zeru_str_eq");
                let equal = self.mk(ExprKind::Call(eq, vec![l, r]), Types::BOOL, span);
                match operator {
                    Token::Eq => equal,
                    _ => self.mk(
                        ExprKind::Unary(UnOp::Not, Box::new(equal)),
                        Types::BOOL,
                        span,
                    ),
                }
            }
            _ => {
                let ty = if comparison { Types::BOOL } else { l_ty };
                let op = binop(operator);
                self.mk(ExprKind::Binary(op, Box::new(l), Box::new(r)), ty, span)
            }
        }
    }

    fn prefix(
        &mut self,
        operator: &Token,
        right: &Expression,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        if *operator == Token::Minus
            && let ExpressionKind::Int(value) = right.kind
        {
            let inner = self.unwrap_expected(expected);
            return self.int_literal(value, true, inner, span);
        }
        let expected = expected.filter(|&ty| !self.cx.types.contains_var(ty));
        match operator {
            Token::Minus => {
                let operand = self.expr(right, expected);
                let types = &self.cx.types;
                match types.as_int(operand.ty) {
                    Some(int) if !int.signed() => {
                        let shown = self.ty(operand.ty);
                        self.error(format!("Cannot negate unsigned type {shown}"), span);
                        self.error_expr(span)
                    }
                    _ if types.is_number(operand.ty) => {
                        let ty = operand.ty;
                        self.mk(ExprKind::Unary(UnOp::Neg, Box::new(operand)), ty, span)
                    }
                    _ if types.is_error(operand.ty) => self.error_expr(span),
                    _ => {
                        let shown = self.ty(operand.ty);
                        self.error(format!("Cannot negate non-numeric type {shown}"), span);
                        self.error_expr(span)
                    }
                }
            }
            Token::Bang => {
                let operand = self.expr(right, expected);
                let types = &self.cx.types;
                if types.is_error(operand.ty) {
                    return self.error_expr(span);
                }
                if operand.ty != Types::BOOL && types.as_int(operand.ty).is_none() {
                    let shown = self.ty(operand.ty);
                    self.error(
                        format!("'!' applies to a bool or an integer, not {shown}"),
                        span,
                    );
                    return self.error_expr(span);
                }
                let ty = operand.ty;
                self.mk(ExprKind::Unary(UnOp::Not, Box::new(operand)), ty, span)
            }
            _ => self.expr(right, expected),
        }
    }

    fn cast(&mut self, value: &Expression, target: &TypeSpec, span: Span) -> Expr {
        let value = self.expr(value, None);
        let scope = self.scope.clone();
        let to = self.cx.resolve_type(target, &scope, span);
        if self.cx.types.is_error(value.ty) || self.cx.types.is_error(to) {
            return self.error_expr(span);
        }
        let carries_data = self
            .cx
            .is_enum(value.ty)
            .is_some_and(|def| self.cx.enum_has_data(def));
        if carries_data || !self.castable(value.ty, to) {
            let (from, to) = (self.ty(value.ty), self.ty(to));
            self.error(format!("Cannot cast {from} to {to}"), span);
            return self.error_expr(span);
        }
        self.mk(ExprKind::Cast(Box::new(value)), to, span)
    }

    /// Whether `as` turns a `from` into a `to`. Nothing becomes an enum: a
    /// number outside its variants would get past an exhaustive `match`. A
    /// slice gives a pointer to its bytes, never a number.
    fn castable(&self, from: Ty, to: Ty) -> bool {
        let types = &self.cx.types;
        let number = |ty| types.is_number(ty);
        let int = |ty| types.as_int(ty).is_some();
        let address = |ty| {
            matches!(
                types.kind(ty),
                TyKind::Ptr(_) | TyKind::Ref(_) | TyKind::RefMut(_)
            )
        };
        let is_ptr = |ty| matches!(types.kind(ty), TyKind::Ptr(_));
        let is_slice = |ty| matches!(types.kind(ty), TyKind::Slice(_));
        (number(from) || self.cx.is_enum(from).is_some()) && number(to)
            || from == Types::BOOL && int(to)
            || address(from) && (int(to) || is_ptr(to))
            || is_slice(from) && is_ptr(to)
            || int(from) && is_ptr(to)
    }

    fn struct_literal(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let Some(def) = self
            .cx
            .items
            .lookup_type(self.scope.module, name)
            .filter(|&def| self.cx.items.item(def).kind == ItemKind::Struct)
        else {
            self.error(format!("Unknown struct type '{name}'"), span);
            for (_, value) in fields {
                self.expr(value, None);
            }
            return self.error_expr(span);
        };
        self.cx.check_visible(def, self.scope.module, span);
        let mut checked: HashMap<usize, Expr> = HashMap::new();
        let Some(ty) = self.literal_type(def, fields, expected, &mut checked, span) else {
            return self.error_expr(span);
        };
        self.cx.note_adt(ty, span);
        let shown = self.ty(ty);

        let declared = self.cx.fields_of(ty);
        let written: Vec<String> = fields.iter().map(|(f, _)| f.clone()).collect();
        if let Some(dup) = super::first_duplicate(&written) {
            self.error(
                format!("Field '{dup}' is given twice in struct literal {shown}"),
                span,
            );
        }
        let mut values = Vec::new();
        for (at, (field, value)) in fields.iter().enumerate() {
            let Some(position) = declared.iter().position(|(d, _)| d == field) else {
                self.error(format!("Unknown field '{field}' in struct '{shown}'"), span);
                continue;
            };
            let field_ty = declared[position].1;
            let checked = checked
                .remove(&at)
                .unwrap_or_else(|| self.value(value, Some(field_ty)));
            let mut checked = self.coerce_or(checked, field_ty, value.span, |want, got| {
                format!(
                    "Type mismatch: Field '{field}' in struct '{shown}' expected {want}, got {got}"
                )
            });
            self.consume(&mut checked);
            values.push((position as u32, checked));
        }
        for (field, _) in &declared {
            if !written.contains(field) {
                self.error(
                    format!("Missing field '{field}' in struct literal {shown}"),
                    span,
                );
            }
        }
        self.mk(ExprKind::Struct(values), ty, span)
    }

    /// The struct a literal stands for. A generic one takes its type
    /// arguments from the expected type, or else from the values given for
    /// fields declared as a bare parameter.
    fn literal_type(
        &mut self,
        def: DefId,
        fields: &[(String, Expression)],
        expected: Option<Ty>,
        checked: &mut HashMap<usize, Expr>,
        span: Span,
    ) -> Option<Ty> {
        let StatementKind::Struct {
            type_params,
            fields: declared,
            ..
        } = &self.cx.items.item(def).decl.kind
        else {
            return None;
        };
        if type_params.is_empty() {
            return Some(self.cx.types.intern(TyKind::Adt(def, Vec::new())));
        }
        if let Some(ty) = expected
            && matches!(self.cx.types.kind(ty), TyKind::Adt(d, _) if *d == def)
        {
            return Some(ty);
        }
        let mut args = Vec::new();
        for param in type_params {
            let spec = TypeSpec::Named(param.name.clone());
            let given = declared
                .iter()
                .find(|(_, declared)| *declared == spec)
                .and_then(|(name, _)| fields.iter().position(|(field, _)| field == name));
            let Some(at) = given else {
                self.error(
                    format!("Cannot tell what '{}' is here, name the type", param.name),
                    span,
                );
                return None;
            };
            let value = self.value(&fields[at].1, None);
            args.push(value.ty);
            checked.insert(at, value);
        }
        let ty = self.cx.generic_adt(def, args, span);
        (!self.cx.types.is_error(ty)).then_some(ty)
    }

    /// An array literal, or with a `Vec<T>` expected, a Vec filled with it.
    fn array_literal(&mut self, elements: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let (hint, into_vec) = self.element_hint(expected);
        let Some((first, rest)) = elements.split_first() else {
            return match expected {
                Some(ty) if hint.is_some() && into_vec => {
                    self.mk(ExprKind::VecLiteral(Vec::new()), ty, span)
                }
                Some(ty) if hint.is_some() => {
                    let elem = hint.unwrap();
                    let ty = if let TyKind::Array(_, 0) = self.cx.types.kind(ty) {
                        ty
                    } else {
                        self.cx.types.intern(TyKind::Array(elem, 0))
                    };
                    self.mk(ExprKind::Array(Vec::new()), ty, span)
                }
                Some(ty) if self.cx.types.is_error(ty) => self.error_expr(span),
                _ => {
                    self.error("Cannot tell the type of [] here; annotate it", span);
                    self.error_expr(span)
                }
            };
        };
        let first = self.value(first, hint);
        let (mut first, elem) = match hint {
            Some(hint) => match self.coerce(first, hint) {
                Ok(first) => (first, hint),
                Err(first) => {
                    let ty = first.ty;
                    (first, ty)
                }
            },
            None => {
                let ty = first.ty;
                (first, ty)
            }
        };
        self.consume(&mut first);
        let mut values = vec![first];
        for (at, element) in rest.iter().enumerate() {
            let value = self.value(element, Some(elem));
            let mut value = self.coerce_or(value, elem, element.span, |want, got| {
                format!(
                    "Array element at index {} type mismatch. Expected {want}, got {got}",
                    at + 1
                )
            });
            self.consume(&mut value);
            values.push(value);
        }
        if into_vec {
            let ty = self.cx.intern_or_error(TyKind::Vec(elem), &[elem]);
            return self.mk(ExprKind::VecLiteral(values), ty, span);
        }
        let ty = self
            .cx
            .intern_or_error(TyKind::Array(elem, values.len() as u64), &[elem]);
        self.mk(ExprKind::Array(values), ty, span)
    }

    fn array_repeat(
        &mut self,
        value: &Expression,
        count: u64,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let (hint, into_vec) = self.element_hint(expected);
        let mut element = self.value(value, hint);
        if let Some(hint) = hint {
            element = self.coerce_or(element, hint, value.span, |want, got| {
                format!("Array element at index 0 type mismatch. Expected {want}, got {got}")
            });
        }
        self.consume(&mut element);
        let elem = element.ty;
        if count > 1 && self.cx.owns_heap(elem) {
            let shown = self.ty(elem);
            self.error(
                format!("[x; N] copies x, and {shown} owns memory; build the elements in a loop"),
                value.span,
            );
        }
        if into_vec {
            let ty = self.cx.intern_or_error(TyKind::Vec(elem), &[elem]);
            return self.mk(ExprKind::VecRepeat(Box::new(element), count), ty, span);
        }
        let ty = self.cx.intern_or_error(TyKind::Array(elem, count), &[elem]);
        self.mk(ExprKind::ArrayRepeat(Box::new(element), count), ty, span)
    }

    /// The element type an expected array or Vec asks for, and whether a
    /// literal builds a Vec.
    fn element_hint(&self, expected: Option<Ty>) -> (Option<Ty>, bool) {
        match expected.map(|ty| self.cx.types.kind(ty)) {
            Some(TyKind::Array(elem, _)) => (Some(*elem), false),
            Some(TyKind::Vec(elem)) => (Some(*elem), true),
            _ => (None, false),
        }
    }

    fn tuple(&mut self, elements: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let wanted = match expected.map(|ty| self.cx.types.kind(ty).clone()) {
            Some(TyKind::Tuple(parts)) => Some(parts),
            _ => None,
        };
        if let Some(wanted) = &wanted
            && wanted.len() != elements.len()
        {
            self.error(
                format!(
                    "Tuple has {} elements, but expected {}",
                    elements.len(),
                    wanted.len()
                ),
                span,
            );
            for element in elements {
                self.expr(element, None);
            }
            return self.error_expr(span);
        }
        let mut values = Vec::new();
        for (at, element) in elements.iter().enumerate() {
            let hint = wanted.as_ref().and_then(|parts| parts.get(at).copied());
            let mut value = self.value(element, hint);
            if let Some(hint) = hint {
                value = match self.coerce(value, hint) {
                    Ok(value) | Err(value) => value,
                };
            }
            self.consume(&mut value);
            values.push(value);
        }
        let parts: Vec<Ty> = values.iter().map(|v| v.ty).collect();
        let ty = self
            .cx
            .intern_or_error(TyKind::Tuple(parts.clone()), &parts);
        self.mk(ExprKind::Tuple(values), ty, span)
    }

    /// `value?` on a `T?`, `value!` on a `T!`: the payload, or a return of
    /// the None or the error, which the function must return in kind.
    fn try_expr(&mut self, operator: &Token, value: &Expression, span: Span) -> Expr {
        let mut value = self.expr(value, None);
        let ok = match (operator, self.cx.types.kind(value.ty)) {
            (Token::Question, TyKind::Optional(ok)) | (Token::Bang, TyKind::Result(ok, _)) => *ok,
            (_, TyKind::Error) => return self.error_expr(span),
            _ => {
                let shown = self.ty(value.ty);
                self.error(
                    format!("'{operator}' takes a T{operator} value, not {shown}"),
                    span,
                );
                return self.error_expr(span);
            }
        };
        let ret = self.ret_ty();
        let passes = matches!(
            (operator, self.cx.types.kind(ret)),
            (Token::Question, TyKind::Optional(_)) | (Token::Bang, TyKind::Result(..))
        );
        if !passes {
            let ret = self.ty(ret);
            self.error(
                format!(
                    "'{operator}' returns from a function returning {ret}, which is no T{operator}"
                ),
                span,
            );
        }
        self.consume(&mut value);
        self.mk(ExprKind::Try(Box::new(value)), ok, span)
    }
}

impl Checker<'_> {
    /// Whether `operator` means something for operands of type `ty`; the
    /// bounds `Eq`, `Ord` and `Num` ask for `==`, `<` and `+`.
    pub(super) fn operator_applies(&mut self, operator: &Token, ty: Ty) -> bool {
        let plain_enum = self.is_enum(ty).is_some_and(|def| !self.enum_has_data(def));
        let types = &self.types;
        let number = types.is_number(ty);
        let int = types.as_int(ty).is_some();
        let kind = types.kind(ty);
        match operator {
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Mod => number,
            Token::PlusWrap | Token::MinusWrap | Token::StarWrap => int,
            Token::BitAnd | Token::BitOr | Token::BitXor => int || *kind == TyKind::Bool,
            Token::ShiftLeft | Token::ShiftRight => int,
            Token::And | Token::Or => *kind == TyKind::Bool,
            Token::Eq | Token::NotEq => {
                number
                    || ty == Types::STR
                    || matches!(kind, TyKind::Bool | TyKind::Ptr(_))
                    || plain_enum
            }
            Token::Lt | Token::Gt | Token::Leq | Token::Geq => {
                number || matches!(kind, TyKind::Ptr(_)) || plain_enum
            }
            _ => true,
        }
    }
}

/// A number written in the source, whose type comes from its context.
pub(super) fn is_number_literal(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Int(_) | ExpressionKind::Float(_) => true,
        ExpressionKind::Prefix {
            operator: Token::Minus,
            right,
        } => is_number_literal(right),
        _ => false,
    }
}

pub(super) fn binop(operator: &Token) -> BinOp {
    match operator {
        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Star => BinOp::Mul,
        Token::Slash => BinOp::Div,
        Token::Mod => BinOp::Rem,
        Token::PlusWrap => BinOp::AddWrap,
        Token::MinusWrap => BinOp::SubWrap,
        Token::StarWrap => BinOp::MulWrap,
        Token::BitAnd => BinOp::BitAnd,
        Token::BitOr => BinOp::BitOr,
        Token::BitXor => BinOp::BitXor,
        Token::ShiftLeft => BinOp::Shl,
        Token::ShiftRight => BinOp::Shr,
        Token::Eq => BinOp::Eq,
        Token::NotEq => BinOp::Ne,
        Token::Lt => BinOp::Lt,
        Token::Leq => BinOp::Le,
        Token::Gt => BinOp::Gt,
        Token::Geq => BinOp::Ge,
        other => unreachable!("'{other}' is not a binary operator"),
    }
}
