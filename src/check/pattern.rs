//! `match`: its patterns, whether they cover every value, and its arms.

use std::collections::HashSet;

use crate::ast::{Expression, ExpressionKind};
use crate::errors::Span;
use crate::items::ItemKind;
use crate::tir::{Arm, Expr, ExprKind, Payload};
use crate::ty::{Ty, TyKind, Types};

use super::body::{FnCx, Moves, always_leaves};

/// Where binding `field` of a pattern for variant `variant` finds its value.
type PayloadAt = fn(u32, u32) -> Payload;

/// What a pattern stands for, to find repeats and gaps, and the names it
/// binds to the values inside.
struct Pattern {
    tag: i128,
    binds: Vec<(String, Ty, Payload)>,
}

impl FnCx<'_, '_> {
    pub(super) fn match_expr(
        &mut self,
        value: &Expression,
        arms: &[(Expression, Expression)],
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

        let binds_any = patterns
            .iter()
            .any(|p| p.as_ref().is_some_and(|p| !p.binds.is_empty()));
        let locked = binds_any.then(|| self.place_path(value)).flatten();
        if let Some(path) = locked.clone() {
            self.lock(path);
        }
        let loans = self.place_loans(&scrutinee, None);
        let before = self.snapshot();
        let mut after = Moves::new();
        let wraps = expected.filter(|&ty| {
            matches!(
                self.cx.types.kind(ty),
                TyKind::Optional(_) | TyKind::Result(..)
            )
        });
        let mut arm_ty: Option<Ty> = wraps;
        let mut checked = Vec::new();
        for ((pattern, body), shape) in arms.iter().zip(patterns) {
            self.restore(&before);
            self.enter();
            let bindings = shape
                .as_ref()
                .map(|shape| {
                    shape
                        .binds
                        .iter()
                        .map(|(name, ty, payload)| {
                            (self.declare(name, *ty, false, true, true), *payload)
                        })
                        .collect()
                })
                .unwrap_or_default();
            for &(local, _) in &bindings {
                self.view_into(local, &scrutinee, pattern.span);
                if self.cx.holds_ref(self.local_ty(local)).is_some() {
                    self.hold(local, loans.clone(), pattern.span);
                }
            }
            let wanted = arm_ty.or(expected);
            let mut result = self.expr(body, wanted);
            self.leave();
            match arm_ty {
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
                            result
                        }
                    }
                }
            }
            self.consume(&mut result);
            let leaves =
                matches!(&body.kind, ExpressionKind::Block(inner) if always_leaves(inner, true));
            if !leaves {
                Self::merge(&mut after, &self.snapshot());
            }
            let tag = if pattern.is_default_pattern() {
                None
            } else {
                Some(shape.map_or(0, |shape| shape.tag))
            };
            checked.push(Arm {
                tag,
                bindings,
                body: result,
            });
        }
        self.restore(&after);
        if locked.is_some() {
            self.unlock();
        }
        let ty = arm_ty.unwrap_or(Types::VOID);
        self.mk(
            ExprKind::Match {
                scrutinee,
                arms: checked,
            },
            ty,
            span,
        )
    }

    /// Whether `name` in a pattern is a variant or a constant, which a
    /// binding would silently shadow.
    fn names_a_value(&self, name: &str) -> bool {
        name.contains("::")
            || self
                .cx
                .items
                .lookup_value(self.scope.module, name)
                .is_some_and(|def| self.cx.items.item(def).kind == ItemKind::Const)
    }

    /// Check each pattern against the subject: none twice, and every value
    /// covered, by a `default` or by listing them all. A value no arm takes
    /// has nowhere to go at runtime.
    fn patterns(
        &mut self,
        subject: Ty,
        arms: &[(Expression, Expression)],
        span: Span,
    ) -> Vec<Option<Pattern>> {
        let none = || {
            arms.iter()
                .map(|(pattern, _)| match &pattern.kind {
                    ExpressionKind::Call { arguments, .. } => Some(Pattern {
                        tag: 0,
                        binds: arguments
                            .iter()
                            .filter_map(|name| match &name.kind {
                                ExpressionKind::Identifier(name) => {
                                    Some((name.clone(), Types::ERROR, Payload::SomeValue))
                                }
                                _ => None,
                            })
                            .collect(),
                    }),
                    _ => None,
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
            if pattern.is_default_pattern() {
                if has_default {
                    self.error("A match has one 'default' arm", pattern.span);
                }
                has_default = true;
                shapes.push(None);
                continue;
            }
            let shape = self.pattern(pattern, subject);
            if let Some(shape) = &shape
                && !covered.insert(shape.tag)
            {
                self.error(
                    "This value is already matched by an earlier arm",
                    pattern.span,
                );
            }
            shapes.push(shape);
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

    /// One pattern: a constant (a number, a bool, a constant's name, a
    /// variant), `None`, `Some(x)`, `Ok(x)`, `Err(e)` or `Shape::Circle(r)`.
    fn pattern(&mut self, pattern: &Expression, subject: Ty) -> Option<Pattern> {
        let subject_kind = self.cx.types.kind(subject).clone();
        let (tag, fields, payload): (i128, Vec<Ty>, PayloadAt) =
            match (&pattern.kind, &subject_kind) {
                (ExpressionKind::None, TyKind::Optional(_)) => {
                    (0, Vec::new(), |_, _| Payload::SomeValue)
                }
                (ExpressionKind::Call { function, .. }, _) => {
                    let ExpressionKind::Identifier(path) = &function.kind else {
                        self.error("A pattern names what it matches", pattern.span);
                        return None;
                    };
                    let found = match (path.as_str(), &subject_kind) {
                        ("Some", TyKind::Optional(inner)) => Some((
                            1,
                            vec![*inner],
                            (|_, _| Payload::SomeValue) as fn(u32, u32) -> Payload,
                        )),
                        ("Ok", TyKind::Result(ok, _)) => {
                            Some((1, vec![*ok], (|_, _| Payload::OkValue) as _))
                        }
                        ("Err", TyKind::Result(_, err)) => {
                            Some((0, vec![*err], (|_, _| Payload::ErrValue) as _))
                        }
                        (path, TyKind::Adt(def, _)) => {
                            path.rsplit_once("::").and_then(|(owner, variant)| {
                                let def = *def;
                                if self.cx.items.lookup_type(self.scope.module, owner) != Some(def)
                                {
                                    return None;
                                }
                                let variants = self.cx.variants_of(def);
                                variants.iter().position(|(v, _)| v == variant).map(|at| {
                                    let payload: fn(u32, u32) -> Payload =
                                        |variant, field| Payload::Variant { variant, field };
                                    (at as i128, variants[at].1.clone(), payload)
                                })
                            })
                        }
                        _ => None,
                    };
                    let Some(found) = found else {
                        let shown = self.ty(subject);
                        self.error(format!("This pattern cannot match {shown}"), pattern.span);
                        return None;
                    };
                    found
                }
                _ => {
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
                        self.error(
                            "A pattern must be a literal, a constant or an enum variant",
                            pattern.span,
                        );
                        return None;
                    };
                    return Some(Pattern {
                        tag,
                        binds: Vec::new(),
                    });
                }
            };

        let names: &[Expression] = match &pattern.kind {
            ExpressionKind::Call { arguments, .. } => arguments,
            _ => &[],
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
                ExpressionKind::Identifier(name) if name == "_" => {}
                ExpressionKind::Identifier(name) if !self.names_a_value(name) => {
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
        Some(Pattern { tag, binds })
    }
}
