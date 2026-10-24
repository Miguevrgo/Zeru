//! `match`: its patterns, whether they cover every value, and its arms.

use std::collections::HashSet;

use crate::ast::{Expression, ExpressionKind, Pattern, PatternKind};
use crate::errors::Span;
use crate::items::ItemKind;
use crate::tir::{Arm, Expr, ExprKind, Payload, Place};
use crate::ty::{Ty, TyKind, Types};

use super::body::FnCx;

/// Where binding `field` of a pattern for variant `variant` finds its value.
type PayloadAt = fn(u32, u32) -> Payload;

/// What an arm's pattern stands for: the values it takes, to find repeats
/// and gaps, and the names it binds, with where each finds its value for
/// each of those values.
struct Shape {
    tags: Vec<i128>,
    binds: Vec<(String, Ty, Vec<Payload>)>,
}

/// One alternative of a pattern: the value it takes and what it binds.
type Alternative = (i128, Vec<(String, Ty, Payload)>);

impl FnCx<'_, '_> {
    pub(super) fn match_expr(
        &mut self,
        value: &Expression,
        arms: &[(Pattern, Expression)],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let (scrutinee, subject) = self.place(value);
        let (scrutinee, subject) = self.through_ref(scrutinee, subject, value.span);
        let patterns = self.patterns(subject, arms, span);
        if arms.is_empty() {
            return self.mk(
                ExprKind::Match {
                    scrutinee,
                    arms: Vec::new(),
                },
                Types::VOID,
                span,
            );
        }

        let before = self.diverges;
        let mut all_leave = true;
        let wraps = expected.filter(|&ty| {
            matches!(
                self.cx.types.kind(ty),
                TyKind::Optional(_) | TyKind::Result(..)
            )
        });
        let mut arm_ty: Option<Ty> = wraps;
        let mut mismatched = false;
        let mut checked = Vec::new();
        let temporary = matches!(scrutinee, Place::Temp(_));
        for ((pattern, body), shape) in arms.iter().zip(patterns) {
            self.diverges = before;
            self.enter();
            let bindings: Vec<_> = shape
                .binds
                .into_iter()
                .map(|(name, ty, payloads)| (self.declare(&name, ty, false, !temporary), payloads))
                .collect();
            let wanted = arm_ty.or(expected);
            let mut result = self.expr(body, wanted);
            for (binding, _) in &bindings {
                self.settle_binding(*binding, &scrutinee);
            }
            let leaves = self.diverges;
            all_leave &= leaves;
            self.leave();
            match arm_ty {
                _ if leaves => {}
                None => arm_ty = Some(result.ty),
                Some(first) => {
                    result = match self.coerce(result, first) {
                        Ok(result) => result,
                        Err(result) => {
                            if !self.cx.types.is_error(result.ty) {
                                let (want, got) = (self.ty(first), self.ty(result.ty));
                                self.error(
                                    format!("Every arm must give {want}, this one gives {got}"),
                                    body.span,
                                );
                            }
                            mismatched = true;
                            result
                        }
                    }
                }
            }
            self.consume(&mut result);
            checked.push(Arm {
                tags: shape.tags,
                bindings,
                body: result,
                span: pattern.span,
            });
        }
        self.diverges = all_leave;
        let ty = match mismatched {
            true => Types::ERROR,
            false => arm_ty.unwrap_or(Types::VOID),
        };
        self.mk(
            ExprKind::Match {
                scrutinee,
                arms: checked,
            },
            ty,
            span,
        )
    }

    /// Whether `name` in a payload is a constant, which a binding would
    /// silently shadow.
    fn names_a_value(&self, name: &str) -> bool {
        self.cx
            .items
            .lookup_value(self.scope.module, name)
            .is_some_and(|def| self.cx.items.item(def).kind == ItemKind::Const)
    }

    /// Check each pattern against the subject: none twice, and every value
    /// covered, by a `default` or by listing them all. A value no arm takes
    /// has nowhere to go at runtime.
    fn patterns(&mut self, subject: Ty, arms: &[(Pattern, Expression)], span: Span) -> Vec<Shape> {
        let none = || {
            arms.iter()
                .map(|(pattern, _)| {
                    let mut names = Vec::new();
                    first_bindings(pattern, &mut names);
                    Shape {
                        tags: Vec::new(),
                        binds: names
                            .into_iter()
                            .map(|name| (name, Types::ERROR, vec![Payload::SomeValue]))
                            .collect(),
                    }
                })
                .collect()
        };
        let all: Option<Vec<String>> = match self.cx.types.kind(subject).clone() {
            TyKind::Error => return none(),
            TyKind::Int(_) => None,
            TyKind::Bool => Some(vec!["false".into(), "true".into()]),
            TyKind::Optional(_) => Some(vec!["None".into(), "Some(..)".into()]),
            TyKind::Result(..) => Some(vec!["Err(..)".into(), "Ok(..)".into()]),
            TyKind::Adt(def, _) if self.cx.is_enum(subject).is_some() => {
                let name = self.cx.items.item(def).name.clone();
                Some(
                    self.cx
                        .variants_of(def)
                        .iter()
                        .map(|(variant, fields)| match fields.is_empty() {
                            true => format!("{name}::{variant}"),
                            false => format!("{name}::{variant}(..)"),
                        })
                        .collect(),
                )
            }
            _ => {
                let shown = self.ty(subject);
                self.error(
                    format!(
                        "Cannot match on {shown}, only on an integer, a bool, an enum, a T? or a T!"
                    ),
                    span,
                );
                return none();
            }
        };

        let mut covered = HashSet::new();
        let mut has_default = false;
        let mut shapes = Vec::new();
        for (pattern, _) in arms {
            if matches!(pattern.kind, PatternKind::Default) {
                if has_default {
                    self.error("A match has one 'default' arm", pattern.span);
                }
                has_default = true;
            }
            shapes.push(self.shape(pattern, subject, &mut covered));
        }

        if has_default {
            return shapes;
        }
        let Some(all) = all else {
            let shown = self.ty(subject);
            self.error(format!("A match on {shown} needs a 'default' arm"), span);
            return shapes;
        };
        let missing: Vec<String> = (0..)
            .zip(all)
            .filter(|(at, _)| !covered.contains(at))
            .map(|(_, name)| name)
            .collect();
        if !missing.is_empty() {
            self.error(
                format!(
                    "Match does not cover {}; add an arm for each or a 'default' arm",
                    missing.join(", ")
                ),
                span,
            );
        }
        shapes
    }

    /// An arm's pattern: each alternative takes values no earlier one took,
    /// and all bind the same names at the same types.
    fn shape(&mut self, pattern: &Pattern, subject: Ty, covered: &mut HashSet<i128>) -> Shape {
        let alternatives = match &pattern.kind {
            PatternKind::Default => &[],
            PatternKind::Or(alternatives) => alternatives.as_slice(),
            _ => std::slice::from_ref(pattern),
        };
        let mut shape: Option<Shape> = None;
        for alternative in alternatives {
            let Some((tag, binds)) = self.pattern(alternative, subject) else {
                continue;
            };
            if !covered.insert(tag) {
                self.error(
                    "This value is already matched by an earlier arm",
                    alternative.span,
                );
            }
            let Some(shape) = &mut shape else {
                shape = Some(Shape {
                    tags: vec![tag],
                    binds: binds
                        .into_iter()
                        .map(|(name, ty, payload)| (name, ty, vec![payload]))
                        .collect(),
                });
                continue;
            };
            let unmatched = shape
                .binds
                .iter()
                .map(|(name, ..)| name)
                .find(|name| !binds.iter().any(|(other, ..)| other == *name))
                .or_else(|| {
                    binds
                        .iter()
                        .map(|(name, ..)| name)
                        .find(|name| !shape.binds.iter().any(|(other, ..)| other == *name))
                });
            if let Some(name) = unmatched {
                let message = format!("'{name}' is not bound in every alternative");
                self.error(message, alternative.span);
                continue;
            }
            for (name, ty, payload) in binds {
                let (_, first, payloads) = shape
                    .binds
                    .iter_mut()
                    .find(|(other, ..)| *other == name)
                    .expect("every alternative binds the same names");
                if *first != ty {
                    let (want, got) = (self.cx.types.show(*first), self.cx.types.show(ty));
                    self.error(
                        format!("'{name}' is {got} here but {want} in the first alternative"),
                        alternative.span,
                    );
                }
                payloads.push(payload);
            }
            shape.tags.push(tag);
        }
        shape.unwrap_or(Shape {
            tags: Vec::new(),
            binds: Vec::new(),
        })
    }

    /// One alternative: a literal, a constant or a variant, `Some(x)`,
    /// `Ok(x)`, `Err(e)` or `Shape::Circle(r)`.
    fn pattern(&mut self, pattern: &Pattern, subject: Ty) -> Option<Alternative> {
        let subject_kind = self.cx.types.kind(subject).clone();
        let (path, names) = match &pattern.kind {
            PatternKind::Payload { path, fields } => (path, fields),
            PatternKind::Constant(Expression {
                kind: ExpressionKind::None,
                ..
            }) if matches!(subject_kind, TyKind::Optional(_)) => return Some((0, Vec::new())),
            PatternKind::Constant(value) => return self.value_pattern(value, subject),
            PatternKind::Binding(_) | PatternKind::Or(_) | PatternKind::Default => {
                unreachable!("the parser puts these only in a payload or as a whole arm")
            }
        };
        let found = match (path.as_str(), &subject_kind) {
            ("Some", TyKind::Optional(inner)) => Some((
                1,
                vec![*inner],
                (|_, _| Payload::SomeValue) as fn(u32, u32) -> Payload,
            )),
            ("Ok", TyKind::Result(ok, _)) => Some((1, vec![*ok], (|_, _| Payload::OkValue) as _)),
            ("Err", TyKind::Result(_, err)) => {
                Some((0, vec![*err], (|_, _| Payload::ErrValue) as _))
            }
            (path, TyKind::Adt(def, _)) => path.rsplit_once("::").and_then(|(owner, variant)| {
                let def = *def;
                if self.cx.items.lookup_type(self.scope.module, owner) != Some(def) {
                    return None;
                }
                let variants = self.cx.variants_of(def);
                variants.iter().position(|(v, _)| v == variant).map(|at| {
                    let payload: PayloadAt = |variant, field| Payload::Variant { variant, field };
                    (at as i128, variants[at].1.clone(), payload)
                })
            }),
            _ => None,
        };
        let Some((tag, fields, payload)) = found else {
            let shown = self.ty(subject);
            self.error(format!("This pattern cannot match {shown}"), pattern.span);
            return None;
        };

        if names.len() != fields.len() {
            self.error(
                format!("This pattern takes {} name(s), one per value", fields.len()),
                pattern.span,
            );
            return None;
        }
        let mut binds = Vec::new();
        for (at, (name, ty)) in names.iter().zip(fields).enumerate() {
            match &name.kind {
                PatternKind::Binding(name) if name == "_" => {}
                PatternKind::Binding(name) if !self.names_a_value(name) => {
                    binds.push((name.clone(), ty, payload(tag as u32, at as u32)));
                }
                _ => {
                    self.error(
                        "A pattern binds names, as in Some(x); match the value inside again",
                        name.span,
                    );
                    return None;
                }
            }
        }
        Some((tag, binds))
    }

    /// A pattern standing for one value, which it is checked and evaluated to.
    fn value_pattern(&mut self, pattern: &Expression, subject: Ty) -> Option<Alternative> {
        let value = self.expr(pattern, Some(subject));
        if self.cx.types.is_error(value.ty) {
            return None;
        }
        if value.ty != subject {
            let (got, want) = (self.ty(value.ty), self.ty(subject));
            self.error(
                format!("A pattern of type {got} cannot match a value of type {want}"),
                pattern.span,
            );
            return None;
        }
        let tag = match &value.kind {
            ExprKind::Variant(index, _) => Some(i128::from(*index)),
            _ => self.cx.eval(&value),
        };
        let Some(tag) = tag else {
            self.error("A pattern must be known at compile time", pattern.span);
            return None;
        };
        Some((tag, Vec::new()))
    }
}

/// The names the first alternative of `pattern` binds.
fn first_bindings(pattern: &Pattern, names: &mut Vec<String>) {
    match &pattern.kind {
        PatternKind::Binding(name) if name != "_" => names.push(name.clone()),
        PatternKind::Payload { fields, .. } => {
            fields.iter().for_each(|field| first_bindings(field, names));
        }
        PatternKind::Or(alternatives) => first_bindings(&alternatives[0], names),
        _ => {}
    }
}
