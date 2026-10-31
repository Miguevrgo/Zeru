//! Expressions: literals, names, operators, casts, literals of structs,
//! arrays and tuples, places, assignment and `?`/`!`.

use std::collections::HashMap;

use crate::ast::{Expression, ExpressionKind, StatementKind, TypeSpec};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::tir::{Arm, Asm, BinOp, Builtin, Expr, ExprKind, Place, UnOp, Unpack};
use crate::token::Token;
use crate::ty::{Ty, TyKind, Types};

use super::body::FnCx;
use super::{Checker, suggest};

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

    /// Report an asm operand of type `ty` that does not fit a register: only
    /// numbers, bools and pointers go in one.
    fn asm_operand(&mut self, ty: Ty, span: Span) {
        let types = &self.cx.types;
        let fits = types.is_number(ty)
            || ty == Types::BOOL
            || types.is_error(ty)
            || matches!(
                types.kind(ty),
                TyKind::Ptr(_) | TyKind::Ref(_) | TyKind::RefMut(_)
            );
        if !fits {
            let shown = self.ty(ty);
            self.error(
                format!("An asm operand goes in a register, so it is a number, a bool or a pointer, not {shown}"),
                span,
            );
        }
    }

    /// The `String` `text` as the `str` its bytes spell, sharing its memory.
    pub(super) fn view(&mut self, text: Expr) -> Expr {
        let span = text.span;
        let place = match text.kind {
            ExprKind::Read(place) => place,
            _ => Place::Temp(Box::new(text)),
        };
        let address = self.address(place, Types::STRING, false, span);
        let from = self.mk(ExprKind::Int(0), Types::USIZE, span);
        self.mk(
            ExprKind::Builtin(Builtin::Slice, vec![address, from]),
            Types::STR,
            span,
        )
    }

    /// `left[start..end]`: the elements of an array, Vec or slice, or the
    /// bytes of a str or String, from `start`, 0 if not written, up to `end`,
    /// the length if not written, sharing their memory. A `mutable` one,
    /// `&var left[..]`, may write them.
    fn slice(
        &mut self,
        left: &Expression,
        start: &Option<Box<Expression>>,
        end: &Option<Box<Expression>>,
        mutable: bool,
        span: Span,
    ) -> Expr {
        let (place, ty) = self.place(left);
        let (place, ty) = self.through_ref(place, ty, left.span);
        if mutable {
            match self.cx.types.kind(ty) {
                TyKind::SliceMut(_) => {}
                TyKind::Slice(_) => {
                    self.error(
                        "Cannot write through a slice, which may point at read-only memory",
                        span,
                    );
                }
                _ => self.require_mutable(&place, left, span),
            }
        }
        let elem = match self.cx.types.elem(ty) {
            Some(elem) => elem,
            None if self.cx.types.is_error(ty) => return self.error_expr(span),
            None => {
                let shown = self.ty(ty);
                self.error(format!("Cannot slice {shown}"), span);
                return self.error_expr(span);
            }
        };
        let bound = |this: &mut Self, bound: &Expression| {
            let value = this.value(bound, Some(Types::USIZE));
            this.coerce_or(value, Types::USIZE, bound.span, |want, got| {
                format!("A slice bound must be {want}, got {got}")
            })
        };
        let from = match start {
            Some(start) => bound(self, start),
            None => self.mk(ExprKind::Int(0), Types::USIZE, span),
        };
        let mut args = vec![self.address(place, ty, mutable, left.span), from];
        if let Some(end) = end {
            args.push(bound(self, end));
        }
        let slice = match mutable {
            true => self.cx.types.intern(TyKind::SliceMut(elem)),
            false => self.cx.types.intern(TyKind::Slice(elem)),
        };
        self.mk(ExprKind::Builtin(Builtin::Slice, args), slice, span)
    }

    /// `Ok()` of the `!` type `ret`.
    pub(super) fn unit_ok(&self, ret: Ty, span: Span) -> Expr {
        let TyKind::Result(unit, _) = *self.cx.types.kind(ret) else {
            unreachable!("a ! type")
        };
        let unit = self.mk(ExprKind::Tuple(Vec::new()), unit, span);
        self.mk(ExprKind::Ok(Box::new(unit)), ret, span)
    }

    pub(super) fn expr(&mut self, e: &Expression, expected: Option<Ty>) -> Expr {
        let expected = expected.map(|ty| self.resolve(ty));
        let errors = self.cx.errors.len();
        let expr = self.expr_inner(e, expected);
        let named = matches!(e.kind, ExpressionKind::Identifier(_));
        if self.cx.errors.len() == errors && !named {
            self.check_constant(&expr);
        }
        expr
    }

    /// `e` where its value is used, which something returning nothing has
    /// not.
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
                Some((_, &TyKind::Result(ok, _)))
                    if matches!(self.cx.types.kind(ok), TyKind::Optional(_)) =>
                {
                    self.mk(ExprKind::None, ok, span)
                }
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
                    self.error("Cannot tell the type of None here", span)
                        .help("give the variable a type, as in var x: i32? = None");
                    self.error_expr(span)
                }
            },
            ExpressionKind::Identifier(name) => self.identifier(name, span),
            ExpressionKind::Slice { left, start, end } => self.slice(left, start, end, false, span),
            ExpressionKind::Get { .. }
            | ExpressionKind::Index { .. }
            | ExpressionKind::Dereference(_) => {
                if let ExpressionKind::Get { object, name } = &e.kind
                    && let Some(limit) = self.limit(object, name, span)
                {
                    return limit;
                }
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
            ExpressionKind::If {
                condition,
                then,
                otherwise,
            } => self.if_expr(condition, then, otherwise, expected, span),
            ExpressionKind::Cast { left, target } => self.cast(left, target, span),
            ExpressionKind::ArrayLiteral(elements) if self.vec_hint(inner).is_some() => {
                self.error("[..] is an array; a Vec is written Vec[..]", span);
                self.vec_literal(elements, inner, span)
            }
            ExpressionKind::ArrayRepeat { value, count } if self.vec_hint(inner).is_some() => {
                self.error("[x; n] is an array; a Vec is written Vec[x; n]", span);
                self.vec_repeat(value, count, inner, span)
            }
            ExpressionKind::ArrayLiteral(elements) => self.array_literal(elements, inner, span),
            ExpressionKind::VecLiteral(elements) => self.vec_literal(elements, inner, span),
            ExpressionKind::VecRepeat { value, count } => {
                self.vec_repeat(value, count, inner, span)
            }
            ExpressionKind::ArrayRepeat { value, count } => {
                let errors = self.cx.errors.len();
                let count = self.value(count, Some(Types::USIZE));
                if self.cx.errors.len() > errors {
                    return self.error_expr(span);
                }
                let Some(count) = self.cx.length(&count) else {
                    return self.error_expr(span);
                };
                self.array_repeat(value, count, inner, span)
            }
            ExpressionKind::Range { .. } => {
                self.error("A range only goes in a 'for' loop", span);
                self.error_expr(span)
            }
            ExpressionKind::Closure(closure) => self.closure(closure, None, span),
            ExpressionKind::Tuple(elements) => {
                let hints = match inner.map(|ty| self.cx.types.kind(ty).clone()) {
                    Some(TyKind::Tuple(parts)) => parts.into_iter().map(Some).collect(),
                    _ => Vec::new(),
                };
                self.tuple(elements, hints, span)
            }
            ExpressionKind::BorrowRef(place) => {
                let mutable = matches!(
                    expected.map(|t| self.cx.types.kind(t)),
                    Some(TyKind::Ptr(_))
                );
                self.borrow(place, mutable, expected, span)
            }
            ExpressionKind::BorrowRefMut(inner) => match &inner.kind {
                ExpressionKind::Slice { left, start, end } => {
                    self.slice(left, start, end, true, span)
                }
                _ => self.borrow(inner, true, expected, span),
            },
            ExpressionKind::InlineAsm {
                template,
                outputs,
                inputs,
                clobbers,
                is_volatile,
            } => {
                let values: Vec<Expr> = inputs.iter().map(|op| self.expr(&op.expr, None)).collect();
                for (value, input) in values.iter().zip(inputs) {
                    self.asm_operand(value.ty, value.span);
                    if input.constraint.is_empty() || input.constraint.starts_with(['=', '+', '~'])
                    {
                        self.error(
                            "An asm input's constraint says where the value goes, as \"r\"",
                            value.span,
                        );
                    }
                }
                let mut places = Vec::new();
                for output in outputs {
                    if output.constraint.len() < 2 || !output.constraint.starts_with('=') {
                        self.error(
                            "An asm output's constraint starts with '=', as \"=r\"",
                            output.expr.span,
                        );
                    }
                    if !matches!(output.expr.kind, ExpressionKind::Identifier(_)) {
                        self.error(
                            "Inline assembly output must be a variable",
                            output.expr.span,
                        );
                    }
                    let (place, ty) = self.place(&output.expr);
                    self.asm_operand(ty, output.expr.span);
                    places.push(place);
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
                self.mk(kind, Types::VOID, span)
            }
        }
    }

    /// A float written out: of the float type its context expects, or f64
    /// when nothing does.
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

    /// An integer written out, negated when `negative`: of the integer type
    /// its context expects, or i32 when nothing does. A value that does not
    /// fit is an error, not a quiet wrap.
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
            let shown = self.ty(ty);
            let help = match by_default {
                true => format!(
                    "a number written alone is {shown}; give it a wider type, as in var x: i64 = ..."
                ),
                false if negative && !int.signed() => format!("{shown} holds no negative numbers"),
                false => format!("{shown} holds {} to {}", int.min(), int.max()),
            };
            self.error(
                format!("Literal {sign}{value} does not fit in {shown}"),
                span,
            )
            .help(help);
        }
        self.mk(ExprKind::Int(signed), ty, span)
    }

    fn identifier(&mut self, name: &str, span: Span) -> Expr {
        if let Some((place, ty)) = self.variable(name) {
            return self.mk(ExprKind::Read(place), ty, span);
        }
        if let Some((owner, variant)) = name.rsplit_once("::")
            && let Some(def) = self.cx.items.lookup_type(self.scope.module, owner)
            && self.cx.items.item(def).kind == ItemKind::Enum
        {
            self.cx.check_visible(def, self.scope.module, span);
            let variants = self.cx.variants_of(def);
            let Some(index) = variants.iter().position(|(v, _)| v == variant) else {
                let names: Vec<String> = variants.iter().map(|(v, _)| v.clone()).collect();
                let at = Span::new(span.end - variant.len(), span.end);
                let error = self.error(format!("Enum '{owner}' has no variant '{variant}'"), span);
                suggest::did_you_mean(error, variant, &names, Some(at));
                if error.help.is_none() {
                    error.help(format!("its variants are {}", names.join(", ")));
                }
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
            Some(def) => {
                let name = name.to_string();
                self.function_value(def, &name, span)
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
                let sources = self.cx.sources;
                let error = self.error(format!("Undeclared variable '{name}'"), span);
                if let Some(ours) = suggest::value_alias(name) {
                    error
                        .help(format!("Zeru writes it '{ours}'"))
                        .fix(span, ours);
                } else if let Some(module) = name
                    .split_once("::")
                    .and_then(|(short, item)| suggest::std_module_with(short, item))
                {
                    suggest::suggest_import(sources, error, span, &module, None);
                } else if let Some(module) = suggest::std_home(name) {
                    suggest::suggest_import(sources, error, span, &module, Some(name));
                } else {
                    suggest::did_you_mean(error, name, &candidates, Some(span));
                }
                self.error_expr(span)
            }
        }
    }

    /// `i32.MAX` or `f64.MIN`: the largest or the lowest value of a number
    /// type, the most negative for a float.
    fn limit(&mut self, object: &Expression, name: &str, span: Span) -> Option<Expr> {
        let ExpressionKind::Identifier(type_name) = &object.kind else {
            return None;
        };
        if !matches!(name, "MAX" | "MIN" | "max" | "min") || self.local(type_name).is_some() {
            return None;
        }
        let ty = self.cx.types.primitive(type_name)?;
        if !self.cx.types.is_number(ty) {
            return None;
        }
        if matches!(name, "max" | "min") {
            let upper = name.to_uppercase();
            self.error(
                format!("A number type's limits are in capitals: {type_name}.{upper}"),
                span,
            );
            return Some(self.error_expr(span));
        }
        let max = name == "MAX";
        let kind = match self.cx.types.kind(ty) {
            TyKind::Int(int) => ExprKind::Int(if max { int.max() } else { int.min() }),
            TyKind::F32 => ExprKind::Float(if max { f32::MAX } else { f32::MIN }.into()),
            TyKind::F64 => ExprKind::Float(if max { f64::MAX } else { f64::MIN }),
            _ => return None,
        };
        Some(self.mk(kind, ty, span))
    }

    /// `if c { a } else { b }` giving a value, checked as a `match` on the
    /// condition; both branches give one type, or the `T?` or `T!` wanted.
    fn if_expr(
        &mut self,
        condition: &Expression,
        then: &Expression,
        otherwise: &Expression,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        self.lint_if(condition, then, otherwise, span);
        let cond = self.condition(condition, "If condition must be boolean, got ");
        let wraps = self.wrapper(expected);
        let before = self.diverges;
        let mut first = self.value_branch(then, expected);
        let first_leaves = std::mem::replace(&mut self.diverges, before);
        let first_ty = (!first_leaves).then_some(first.ty);
        let second = self.value_branch(otherwise, wraps.or(first_ty).or(expected));
        let second_leaves = self.diverges;
        self.diverges = first_leaves && second_leaves;
        let second_ty = (!second_leaves).then_some(second.ty);
        let target = wraps.or(first_ty).or(second_ty).unwrap_or(Types::VOID);
        if !first_leaves {
            first = self.coerce(first, target).unwrap_or_else(|first| first);
        }
        let mut second = match self.coerce(second, target) {
            Ok(second) => second,
            Err(second) if second_leaves => second,
            Err(second) => {
                let closures =
                    [target, second.ty].map(|ty| self.cx.types.closure_sig(ty).is_some());
                if closures == [true, true] {
                    self.error(
                        "Each closure is a type of its own, so the branches of an if cannot give two",
                        otherwise.span,
                    )
                    .help("call the closure each branch needs inside that branch");
                } else if !self.cx.types.is_error(second.ty) && !self.cx.types.is_error(target) {
                    let (want, got) = (self.ty(target), self.ty(second.ty));
                    self.error(
                        format!("Both branches of the if must give {want}; this one gives {got}"),
                        otherwise.span,
                    );
                }
                second
            }
        };
        self.consume(&mut first);
        self.consume(&mut second);
        let arms = vec![
            Arm {
                tags: vec![(1, 1)],
                bindings: Vec::new(),
                guard: None,
                body: first,
                span: then.span,
            },
            Arm {
                tags: Vec::new(),
                bindings: Vec::new(),
                guard: None,
                body: second,
                span: otherwise.span,
            },
        ];
        let scrutinee = Place::Temp(Box::new(cond));
        self.mk(ExprKind::Match { scrutinee, arms }, target, span)
    }

    /// One branch of an `if` giving a value. A block in its place must
    /// leave with `return`, `break` or `continue`.
    fn value_branch(&mut self, branch: &Expression, expected: Option<Ty>) -> Expr {
        let checked = self.expr(branch, expected);
        if matches!(branch.kind, ExpressionKind::Block(_)) && !self.diverges {
            self.error(
                "A branch of an if that gives a value holds one expression, or leaves with return, break or continue",
                branch.span,
            );
            return self.error_expr(branch.span);
        }
        checked
    }

    /// Where `e` lives: a local, a field, an element, what a pointer points
    /// at, or, for a value with no place of its own, a temporary.
    pub(super) fn place(&mut self, e: &Expression) -> (Place, Ty) {
        self.place_expecting(e, None)
    }

    /// The same, a temporary taking the type `expected` when it can.
    fn place_expecting(&mut self, e: &Expression, expected: Option<Ty>) -> (Place, Ty) {
        let span = e.span;
        if let ExpressionKind::Identifier(name) = &e.kind
            && self.local(name).is_none()
            && let Some((at, ty)) = self.capture(name)
        {
            return (Place::Capture(at), ty);
        }
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
                let pointee = match self.cx.types.kind(pointer.ty).clone() {
                    TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => inner,
                    TyKind::Error => Types::ERROR,
                    kind => {
                        let shown = self.ty(pointer.ty);
                        let inner = self.cx.sources.text(pointer.span).to_string();
                        let wrapped = matches!(kind, TyKind::Optional(_) | TyKind::Result(..));
                        let error = self.error(format!("Cannot dereference type {shown}"), span);
                        match wrapped {
                            true => error
                                .help("take the reference out first, then read through it")
                                .fix(span, format!("*{inner}.unwrap()")),
                            false => error
                                .help("only a reference or a pointer is read through with *")
                                .fix(span, inner),
                        };
                        Types::ERROR
                    }
                };
                (Place::Deref(Box::new(pointer)), pointee)
            }
            _ => {
                let value = self.expr(e, expected);
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
                    let last = parts.len().saturating_sub(1);
                    self.error(format!("{shown} has no field '{name}'"), span)
                        .help(format!(
                            "a tuple's parts are named by position, .0 to .{last}"
                        ));
                    (base, Types::ERROR)
                }
            },
            TyKind::Adt(..) if self.cx.is_struct(ty).is_some() => {
                let fields = self.cx.fields_of(ty);
                match fields.iter().position(|(field, _)| field == name) {
                    Some(at) => (Place::Field(Box::new(base), at as u32), fields[at].1),
                    None => {
                        let names: Vec<String> = fields.into_iter().map(|(f, _)| f).collect();
                        self.no_field(ty, name, &names, span);
                        (base, Types::ERROR)
                    }
                }
            }
            _ => {
                self.no_field(ty, name, &[], span);
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
            TyKind::Vec(elem) | TyKind::Slice(elem) | TyKind::SliceMut(elem) => elem,
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
    /// field, an element or a method is reached through a `&T`.
    pub(super) fn through_ref(&mut self, place: Place, ty: Ty, span: Span) -> (Place, Ty) {
        match self.cx.types.kind(ty) {
            TyKind::Ref(_) | TyKind::RefMut(_) => self.deref(place, ty, span),
            _ => (place, ty),
        }
    }

    fn borrow(
        &mut self,
        inner: &Expression,
        mutable: bool,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let pointee = expected.and_then(|ty| match self.cx.types.kind(ty) {
            TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => Some(*inner),
            _ => None,
        });
        let (place, ty) = self.place_expecting(inner, pointee);
        if mutable {
            self.require_mutable(&place, inner, span);
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
        if let ExpressionKind::Tuple(targets) = &target.kind {
            if *operator == Token::Assign {
                return self.assign_tuple(targets, value, span);
            }
            self.error(
                format!("Only '=' sets several places at once, not '{operator}'"),
                span,
            );
            self.expr(value, None);
            return self.error_expr(span);
        }
        if *operator == Token::Assign && same(target, value) {
            self.warn("This assigns a value to where it already is", span);
        }
        let (place, target_ty) = self.target(target, span);
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
                let written = self.cx.sources.text(target.span).to_string();
                self.mismatch(&value, target_ty, span, |want, got| {
                    format!("'{written}' holds {want}, but this is {got}")
                });
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

    /// The place an assignment writes, checked to be one it may write.
    fn target(&mut self, target: &Expression, span: Span) -> (Place, Ty) {
        let revived = match &target.kind {
            ExpressionKind::Identifier(name) => self.local(name),
            _ => None,
        };
        let (place, target_ty) = match revived {
            Some(local) => (Place::Local(local), self.local_ty(local)),
            None => self.place(target),
        };
        if self.temporary(&place, target_ty, target) {
            self.error("Cannot assign to a temporary value", target.span)
                .help("store the value in a variable first, then change the variable");
        }
        match revived {
            Some(local) => self.require_var(local, span),
            None => self.require_mutable(&place, target, span),
        }
        (place, target_ty)
    }

    /// `(a, b) = value`: all of `value` is worked out first, then each part
    /// is stored in its place in turn; `_` leaves a part out.
    fn assign_tuple(&mut self, targets: &[Expression], value: &Expression, span: Span) -> Expr {
        let mut stored = Vec::new();
        let (target, hints) = self.stores(targets, &mut stored, span);
        let mut value = self.unpacked(value, hints);
        self.consume(&mut value);
        self.check_stores(&target, value.ty, &mut stored.into_iter(), span);
        let kind = ExprKind::Unpack {
            target,
            value: Box::new(value),
        };
        self.mk(kind, Types::VOID, span)
    }

    /// `value` worked out against the part types `hints`: a tuple literal
    /// part by part, anything else whole.
    pub(super) fn unpacked(&mut self, value: &Expression, hints: Vec<Option<Ty>>) -> Expr {
        match &value.kind {
            ExpressionKind::Tuple(items) if items.len() == hints.len() => {
                self.tuple(items, hints, value.span)
            }
            _ => {
                let whole = self.tuple_hint(hints);
                self.value(value, whole)
            }
        }
    }

    /// `expected` when it is a `T?` or a `T!`, which a plain value wraps
    /// into.
    pub(super) fn wrapper(&self, expected: Option<Ty>) -> Option<Ty> {
        expected.filter(|&ty| {
            matches!(
                self.cx.types.kind(ty),
                TyKind::Optional(_) | TyKind::Result(..)
            )
        })
    }

    /// The tuple type of the part types `hints`, when each is known.
    fn tuple_hint(&mut self, hints: Vec<Option<Ty>>) -> Option<Ty> {
        let parts = hints.into_iter().collect::<Option<Vec<Ty>>>()?;
        Some(self.cx.types.intern(TyKind::Tuple(parts)))
    }

    /// Where each of `targets` stores its part, with the part's type when
    /// known; the type of each place stored to goes to `stored`, in order.
    fn stores(
        &mut self,
        targets: &[Expression],
        stored: &mut Vec<Ty>,
        span: Span,
    ) -> (Unpack, Vec<Option<Ty>>) {
        let mut parts = Vec::new();
        let mut hints = Vec::new();
        for target in targets {
            match &target.kind {
                ExpressionKind::Identifier(name) if name == "_" => {
                    parts.push(Unpack::Skip);
                    hints.push(None);
                }
                ExpressionKind::Tuple(inner) => {
                    let (part, inner) = self.stores(inner, stored, span);
                    hints.push(self.tuple_hint(inner));
                    parts.push(part);
                }
                _ => {
                    let (place, ty) = self.target(target, span);
                    stored.push(ty);
                    parts.push(Unpack::Store(place));
                    hints.push(Some(ty));
                }
            }
        }
        (Unpack::Tuple(parts), hints)
    }

    /// Check that a value of type `ty` fits `target`, part by part.
    fn check_stores(
        &mut self,
        target: &Unpack,
        ty: Ty,
        stored: &mut impl Iterator<Item = Ty>,
        span: Span,
    ) {
        match target {
            Unpack::Tuple(parts) => {
                let tys = self.parts(parts.len(), ty, span);
                for (part, ty) in parts.iter().zip(tys) {
                    self.check_stores(part, ty, stored, span);
                }
            }
            Unpack::Store(_) => {
                let want = stored.next().expect("a type for each place stored to");
                if want != ty && !self.cx.types.is_error(ty) && !self.cx.types.is_error(want) {
                    let (want, got) = (self.ty(want), self.ty(ty));
                    self.error(
                        format!("This place holds {want}, but its part of the value is {got}"),
                        span,
                    );
                }
            }
            Unpack::Skip | Unpack::Declare(_) => {}
        }
    }

    /// Whether `place` is what a raw `*T` points at, which may hold no value
    /// yet: writing there drops nothing.
    fn writes_raw(&self, place: &Place) -> bool {
        matches!(place, Place::Deref(pointer) if matches!(self.cx.types.kind(pointer.ty), TyKind::Ptr(_)))
    }

    /// The constant whose value `place`, written as `ast`, is part of. A
    /// constant is a value and not a place, so a write to it
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
                self.cx.types.elem(base)?
            }
            Place::Deref(pointer) => match self.cx.types.kind(pointer.ty) {
                TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                _ => return None,
            },
            Place::Temp(value) => value.ty,
            Place::Capture(at) => self.captures[*at as usize].outer.ty,
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
        let (mut l, mut r) = self.operands(left, right, hint);
        if comparison {
            if l.ty == Types::STRING {
                l = self.view(l);
            }
            if r.ty == Types::STRING {
                r = self.view(r);
            }
        }
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
                let r_ty = match &r.kind {
                    ExprKind::Some(inner) | ExprKind::Ok(inner) => inner.ty,
                    _ => r.ty,
                };
                let wrapped = [l.ty, r_ty].into_iter().any(|ty| {
                    matches!(
                        self.cx.types.kind(ty),
                        TyKind::Optional(_) | TyKind::Result(..)
                    )
                });
                let (lt, rt) = (self.ty(l.ty), self.ty(r_ty));
                let verb = match operator.to_string().trim_end_matches('%') {
                    "+" => "add",
                    "-" => "subtract",
                    "*" => "multiply",
                    "/" => "divide",
                    "" => "take the remainder of",
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => "compare",
                    _ => "combine",
                };
                let types = &self.cx.types;
                let numbers = types.is_number(l.ty) && types.is_number(r_ty);
                let width = |ty| match types.as_int(ty) {
                    Some(int) => int.bits(),
                    None if ty == Types::F32 => 100,
                    None => 200,
                };
                let (convert, into) = match width(r_ty) > width(l.ty) {
                    true => (&l, rt.clone()),
                    false => (&r, lt.clone()),
                };
                let sources = self.cx.sources;
                let text = sources.text(convert.span);
                let operand = match text
                    .chars()
                    .all(|c| c.is_alphanumeric() || "_.[]()".contains(c))
                {
                    true => text.to_string(),
                    false => format!("({text})"),
                };
                let fix = format!("{operand} as {into}");
                let convert_span = convert.span;
                let wrapped_side = [(&l, l.ty), (&r, r_ty)].into_iter().find(|(_, ty)| {
                    matches!(types.kind(*ty), TyKind::Optional(_) | TyKind::Result(..))
                });
                let unwrapped = wrapped_side.map(|(side, _)| {
                    let text = sources.text(side.span);
                    (side.span, format!("{text}.unwrap()"))
                });
                let error = self
                    .error(format!("Cannot {verb} {lt} and {rt}"), r.span)
                    .label(format!("this is {rt}"));
                error.note(format!("this is {lt}"), l.span);
                if wrapped {
                    error.help("take the value out first, with !, ? or .unwrap()");
                    if let Some((at, fix)) = unwrapped {
                        error.fix(at, fix);
                    }
                } else if numbers {
                    error
                        .help("make them one type: convert one with as")
                        .fix(convert_span, fix);
                }
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
        if comparison {
            self.lint_comparison(left, operator, right, &l, l_ty, span);
        }
        match operator {
            Token::And => self.mk(ExprKind::And(Box::new(l), Box::new(r)), Types::BOOL, span),
            Token::Or => self.mk(ExprKind::Or(Box::new(l), Box::new(r)), Types::BOOL, span),
            Token::Lt | Token::Gt | Token::Leq | Token::Geq if l_ty == Types::STR => {
                let cmp = self.cx.prelude_fn("__zeru_str_cmp");
                let order = self.mk(ExprKind::Call(cmp, vec![l, r]), Types::I32, span);
                let zero = self.mk(ExprKind::Int(0), Types::I32, span);
                let op = binop(operator);
                self.mk(
                    ExprKind::Binary(op, Box::new(order), Box::new(zero)),
                    Types::BOOL,
                    span,
                )
            }
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

    /// Warn about a comparison whose answer is known before it runs, or
    /// that a shorter form says.
    fn lint_comparison(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        l: &Expr,
        ty: Ty,
        span: Span,
    ) {
        let zero = |e: &Expression| matches!(e.kind, ExpressionKind::Int(0));
        let unsigned = self.cx.types.as_int(ty).is_some_and(|int| !int.signed());
        let always = match operator {
            _ if same(left, right) && !self.cx.types.is_float(ty) => {
                Some(matches!(operator, Token::Eq | Token::Leq | Token::Geq))
            }
            Token::Lt if unsigned && zero(right) => Some(false),
            Token::Gt if unsigned && zero(left) => Some(false),
            Token::Geq if unsigned && zero(right) => Some(true),
            Token::Leq if unsigned && zero(left) => Some(true),
            _ => None,
        };
        let equality = matches!(operator, Token::Eq | Token::NotEq);
        let flag = [left, right]
            .iter()
            .any(|e| matches!(e.kind, ExpressionKind::Boolean(_)));
        let types = &self.cx.types;
        let vec_len = match &l.kind {
            ExprKind::Builtin(Builtin::Len, args) => match types.kind(args[0].ty) {
                TyKind::Ref(held) | TyKind::RefMut(held) | TyKind::Ptr(held) => {
                    matches!(types.kind(*held), TyKind::Vec(_))
                }
                kind => matches!(kind, TyKind::Vec(_)),
            },
            _ => false,
        };
        let sources = self.cx.sources;
        if let Some(holds) = always {
            self.warn(format!("This comparison is always {holds}"), span);
        } else if equality && flag {
            let (value, literal) = match left.kind {
                ExpressionKind::Boolean(b) => (right, b),
                _ => (left, matches!(right.kind, ExpressionKind::Boolean(true))),
            };
            let text = sources.text(value.span);
            let same = literal == (*operator == Token::Eq);
            let written = match same {
                true => text.to_string(),
                false => format!("!{text}"),
            };
            self.warn(
                "A bool compared with true or false is itself or its opposite",
                span,
            )
            .help("drop the comparison")
            .fix(span, written);
        } else if vec_len && zero(right) && (equality || *operator == Token::Gt) {
            let warning = self.warn("Write .is_empty() to ask whether a Vec is empty", span);
            if let ExpressionKind::Call { function, .. } = &left.kind
                && let ExpressionKind::Get { object, .. } = &function.kind
            {
                let negated = match operator {
                    Token::Eq => "",
                    _ => "!",
                };
                let text = sources.text(object.span);
                warning
                    .help("ask it directly")
                    .fix(span, format!("{negated}{text}.is_empty()"));
            }
        }
    }

    /// Warn about an `if` giving a value that a shorter form says: the
    /// condition itself, or `min`, `max` or `abs` from std.math.
    fn lint_if(
        &mut self,
        condition: &Expression,
        then: &Expression,
        otherwise: &Expression,
        whole: Span,
    ) {
        use ExpressionKind as E;
        let negated = |e: &Expression, of: &Expression| matches!(&e.kind, E::Prefix { operator: Token::Minus, right } if same(right, of));
        let text = self.cx.sources.text(condition.span).to_string();
        let message = match (&condition.kind, &then.kind, &otherwise.kind) {
            (_, E::Boolean(true), E::Boolean(false)) => {
                self.warn("This if gives its condition", whole)
                    .help("write the condition alone")
                    .fix(whole, text);
                return;
            }
            (_, E::Boolean(false), E::Boolean(true)) => {
                self.warn("This if gives the opposite of its condition", whole)
                    .help("write the condition negated")
                    .fix(whole, format!("!({text})"));
                return;
            }
            (
                E::Infix {
                    left,
                    operator: operator @ (Token::Lt | Token::Gt | Token::Leq | Token::Geq),
                    right,
                },
                _,
                _,
            ) => {
                let greater = matches!(operator, Token::Gt | Token::Geq);
                let zero = matches!(right.kind, E::Int(0));
                if same(then, left) && same(otherwise, right) {
                    match greater {
                        true => "This if picks the larger value; write max(a, b) from std.math",
                        false => "This if picks the smaller value; write min(a, b) from std.math",
                    }
                } else if same(then, right) && same(otherwise, left) {
                    match greater {
                        true => "This if picks the smaller value; write min(a, b) from std.math",
                        false => "This if picks the larger value; write max(a, b) from std.math",
                    }
                } else if zero
                    && (greater && same(then, left) && negated(otherwise, left)
                        || !greater && negated(then, left) && same(otherwise, left))
                {
                    "This if gives the absolute value; write abs(x) from std.math"
                } else {
                    return;
                }
            }
            _ => return,
        };
        self.warn(message, condition.span.merge(otherwise.span));
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
                        self.error(format!("Cannot negate unsigned type {shown}"), span)
                            .help(format!("{shown} holds no negative numbers; convert it to a signed type first, as in -(x as i64)"));
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
            let text = self.cx.sources.text(value.span).to_string();
            let types = &self.cx.types;
            let (got, target) = (value.ty, to);
            let text_like = got == Types::STR || got == Types::STRING;
            let number = types.is_number(got);
            let to_number = types.is_number(target);
            let zero = match types.is_float(got) {
                true => "0.0",
                false => "0",
            };
            let to_float = types.is_float(target);
            let enum_target = self.cx.is_enum(target).is_some();
            let (from, to) = (self.ty(got), self.ty(target));
            let error = self.error(format!("Cannot cast {from} to {to}"), span);
            if text_like && to_number {
                error.help("read a number out of text with .parse_int() or .parse_float()");
            } else if number && target == Types::BOOL {
                error
                    .help("compare it to get a bool")
                    .fix(span, format!("{text} != {zero}"));
            } else if got == Types::BOOL && to_float {
                error.help("pick the number with if, as in if b { 1.0 } else { 0.0 }");
            } else if number && enum_target {
                error.help("a number does not become an enum; match it to pick a variant");
            } else if carries_data {
                error.help("only an enum whose variants carry no values casts to a number");
            }
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
        let is_slice = |ty| matches!(types.kind(ty), TyKind::Slice(_) | TyKind::SliceMut(_));
        (number(from) || self.cx.is_enum(from).is_some()) && number(to)
            || from == Types::BOOL && int(to)
            || address(from) && (int(to) || is_ptr(to))
            || is_slice(from) && is_ptr(to)
            || int(from) && is_ptr(to)
    }

    /// Report that `ty` has no field `name`, read at the end of `span`,
    /// with what was meant when it can tell: a method called without
    /// parentheses, or a field of a similar name.
    fn no_field(&mut self, ty: Ty, name: &str, fields: &[String], span: Span) {
        let sources = self.cx.sources;
        let at =
            Some(Span::new(span.end - name.len(), span.end)).filter(|&at| sources.text(at) == name);
        let method = self.methods_of(ty).iter().any(|m| m == name);
        let shown = self.ty(ty);
        let what = match self.cx.is_struct(ty) {
            Some(_) => format!("Struct '{shown}'"),
            None => shown,
        };
        let error = self.error(format!("{what} has no field '{name}'"), at.unwrap_or(span));
        if method {
            error.help(format!("'{name}' is a method; call it"));
            if let Some(at) = at {
                error.fix(at, format!("{name}()"));
            }
        } else if fields.is_empty() {
            error.help("only a struct or a tuple has fields");
        } else {
            suggest::did_you_mean(error, name, fields, at);
            if error.help.is_none() {
                error.help(format!("its fields are {}", fields.join(", ")));
            }
        }
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
            let sources = self.cx.sources;
            let at = sources.word_in(span, name);
            let structs = self
                .cx
                .items
                .names_of(self.scope.module, ItemKind::Struct, false);
            let error = self.error(format!("Unknown struct type '{name}'"), at.unwrap_or(span));
            match suggest::std_home(name) {
                Some(module) => suggest::suggest_import(sources, error, span, &module, Some(name)),
                None => suggest::did_you_mean(error, name, &structs, at),
            }
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
        let sources = self.cx.sources;
        let name_of = |at: usize| {
            let start = match at {
                0 => span.start,
                _ => fields[at - 1].1.span.end,
            };
            sources.last_word_in(Span::new(start, fields[at].1.span.start), &fields[at].0)
        };
        if let Some(dup) = super::first_duplicate(&written) {
            let mut given = written.iter().enumerate().filter(|(_, f)| *f == dup);
            let first = given.next().and_then(|(at, _)| name_of(at));
            let again = given.next().and_then(|(at, _)| name_of(at));
            let error = self.error(
                format!("Field '{dup}' is given twice in struct literal {shown}"),
                again.unwrap_or(span),
            );
            if let Some(first) = first {
                error.note("first given here", first);
            }
        }
        let missing: Vec<String> = declared
            .iter()
            .map(|(d, _)| d.clone())
            .filter(|d| !written.contains(d))
            .collect();
        let mut meant = Vec::new();
        let mut values = Vec::new();
        for (at, (field, value)) in fields.iter().enumerate() {
            let Some(position) = declared.iter().position(|(d, _)| d == field) else {
                let named = name_of(at);
                let error = self.error(
                    format!("Unknown field '{field}' in struct '{shown}'"),
                    named.unwrap_or(span),
                );
                suggest::did_you_mean(error, field, &missing, named);
                meant.extend(suggest::closest(field, &missing).map(String::from));
                self.expr(value, None);
                continue;
            };
            let field_ty = declared[position].1;
            let checked = checked
                .remove(&at)
                .unwrap_or_else(|| self.value(value, Some(field_ty)));
            let mut checked = self.coerce_or(checked, field_ty, value.span, |want, got| {
                format!("Field '{field}' of '{shown}' is {want}, but this is {got}")
            });
            self.consume(&mut checked);
            values.push((position as u32, checked));
        }
        let missing: Vec<String> = missing.into_iter().filter(|f| !meant.contains(f)).collect();
        match missing.as_slice() {
            [] => {}
            [field] => {
                self.error(
                    format!("Missing field '{field}' in struct literal {shown}"),
                    span,
                )
                .help(format!("give it a value: {field}: ..."));
            }
            _ => {
                let names = missing.iter().map(|f| format!("'{f}'")).collect::<Vec<_>>();
                self.error(
                    format!(
                        "Missing fields {} in struct literal {shown}",
                        names.join(", ")
                    ),
                    span,
                )
                .help("give each of them a value");
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

    /// `Vec[a, b]`: a Vec holding these elements.
    fn vec_literal(&mut self, elements: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let hint = self.vec_hint(expected);
        if elements.is_empty() {
            return match hint {
                Some(elem) => {
                    let ty = self.cx.intern_or_error(TyKind::Vec(elem), &[elem]);
                    self.mk(ExprKind::VecLiteral(Vec::new()), ty, span)
                }
                None => {
                    self.error("Cannot tell what this Vec holds", span)
                        .help("give the variable a type, as in var v: Vec<i32> = Vec[]");
                    self.error_expr(span)
                }
            };
        }
        let count = elements.len() as u64;
        let wanted = hint.map(|elem| self.cx.types.intern(TyKind::Array(elem, count)));
        let array = self.array_literal(elements, wanted, span);
        let ExprKind::Array(values) = array.kind else {
            return array;
        };
        let elem = values[0].ty;
        let ty = self.cx.intern_or_error(TyKind::Vec(elem), &[elem]);
        self.mk(ExprKind::VecLiteral(values), ty, span)
    }

    /// `Vec[x; n]`: a Vec of `n` copies of `x`, `n` known as it runs. A value
    /// that owns memory is copied into each place, as `.copy()` would.
    fn vec_repeat(
        &mut self,
        value: &Expression,
        count: &Expression,
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let hint = self.vec_hint(expected);
        let count = self.value(count, Some(Types::USIZE));
        let count = self.coerce_or(count, Types::USIZE, span, |want, got| {
            format!("Vec[x; n] takes n as {want}, not {got}")
        });
        let mut element = self.value(value, hint);
        if let Some(hint) = hint {
            element = self.coerce_or(element, hint, value.span, |want, got| {
                format!("This element is {got}, but the Vec holds {want}")
            });
        }
        let elem = element.ty;
        if self.cx.owns_heap(elem) {
            self.copies_drop(elem, value.span);
        } else {
            self.consume(&mut element);
        }
        let ty = self.cx.intern_or_error(TyKind::Vec(elem), &[elem]);
        self.mk(
            ExprKind::VecRepeat(Box::new(element), Box::new(count)),
            ty,
            span,
        )
    }

    /// The element type an expected `Vec<T>` asks for.
    fn vec_hint(&self, expected: Option<Ty>) -> Option<Ty> {
        match expected.map(|ty| self.cx.types.kind(ty)) {
            Some(TyKind::Vec(elem)) => Some(*elem),
            _ => None,
        }
    }

    /// An array literal.
    fn array_literal(&mut self, elements: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let hint = self.element_hint(expected);
        let Some((first, rest)) = elements.split_first() else {
            return match expected {
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
                    self.error("Cannot tell what this array holds", span)
                        .help("give the variable a type, as in var a: Array<i32, 0> = []");
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
                format!("Element {} is {got}, but the array holds {want}", at + 1)
            });
            self.consume(&mut value);
            values.push(value);
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
        let hint = self.element_hint(expected);
        let mut element = self.value(value, hint);
        if let Some(hint) = hint {
            element = self.coerce_or(element, hint, value.span, |want, got| {
                format!("This element is {got}, but the array holds {want}")
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
        let ty = self.cx.intern_or_error(TyKind::Array(elem, count), &[elem]);
        self.mk(ExprKind::ArrayRepeat(Box::new(element), count), ty, span)
    }

    /// The element type an expected array asks for.
    fn element_hint(&self, expected: Option<Ty>) -> Option<Ty> {
        match expected.map(|ty| self.cx.types.kind(ty)) {
            Some(TyKind::Array(elem, _)) => Some(*elem),
            _ => None,
        }
    }

    /// A tuple literal, each element checked against its part of `hints`.
    pub(super) fn tuple(
        &mut self,
        elements: &[Expression],
        hints: Vec<Option<Ty>>,
        span: Span,
    ) -> Expr {
        let wanted = (!hints.is_empty()).then_some(hints);
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
            let hint = wanted
                .as_ref()
                .and_then(|parts| parts.get(at).copied().flatten());
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
        if let Token::Eq | Token::NotEq | Token::Lt | Token::Gt | Token::Leq | Token::Geq = operator
            && let Some(parts) = self.parts_of(ty)
        {
            return parts
                .into_iter()
                .all(|part| self.operator_applies(operator, part));
        }
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
                    || ty == Types::STRING
                    || matches!(kind, TyKind::Bool | TyKind::Ptr(_))
                    || plain_enum
            }
            Token::Lt | Token::Gt | Token::Leq | Token::Geq => {
                number
                    || ty == Types::STR
                    || ty == Types::STRING
                    || matches!(kind, TyKind::Ptr(_))
                    || plain_enum
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

/// Whether `a` and `b` read the same value: the same variable, field,
/// element or literal, written the same way.
fn same(a: &Expression, b: &Expression) -> bool {
    use ExpressionKind as E;
    match (&a.kind, &b.kind) {
        (E::Identifier(x), E::Identifier(y)) => x == y,
        (E::Int(x), E::Int(y)) => x == y,
        (E::Boolean(x), E::Boolean(y)) => x == y,
        (E::Get { object: x, name: m }, E::Get { object: y, name: n }) => m == n && same(x, y),
        (E::Index { left: x, index: i }, E::Index { left: y, index: j }) => {
            same(x, y) && same(i, j)
        }
        (E::Dereference(x), E::Dereference(y)) => same(x, y),
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
