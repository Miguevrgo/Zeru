//! `match`: its patterns, whether they cover every value, and its arms.

use crate::ast::{Expression, ExpressionKind, MatchArm, Pattern, PatternKind};
use crate::errors::Span;
use crate::items::ItemKind;
use crate::tir::{Arm, Expr, ExprKind, Payload, Place, Reach, Test};
use crate::token::Token;
use crate::ty::{Ty, TyKind, Types};

use super::body::{Fixed, FnCx};

/// The binding a match on text tests its values against, a name no code
/// can write.
const TEXT: &str = "the text matched";

/// Where binding `field` of a pattern for variant `variant` finds its value.
type PayloadAt = fn(u32, u32) -> Payload;

/// What an arm's pattern stands for: the values it takes, each alternative
/// the tests that must all hold, to find repeats and gaps, and the names it
/// binds, with where each finds its value for each alternative.
#[derive(Default)]
struct Shape {
    tags: Vec<Vec<Test>>,
    binds: Vec<(String, Ty, Vec<Reach>)>,
}

/// One alternative of a pattern: the values it takes, from the first to
/// the second, and what it binds.
type Alternative = ((i128, i128), Vec<(String, Ty, Payload)>);

/// The names an alternative binds, with their types and where each finds
/// its value.
type Binds = Vec<(String, Ty, Reach)>;

/// One alternative of a whole pattern: the tests that must all hold, and
/// what it binds.
type Tested = (Vec<Test>, Binds);

/// The tag a tuple element stands at in a combination of tags when a match
/// does not list its values, as an integer's: only a name or `_` takes it.
const OTHER: i128 = i128::MIN;

/// How many combinations of its elements' values a match on a tuple checks
/// at most for gaps and repeats; beyond them it needs a `default`.
const MAX_COMBINATIONS: usize = 4096;

impl FnCx<'_, '_> {
    pub(super) fn match_expr(
        &mut self,
        value: &Expression,
        arms: &[MatchArm],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let (scrutinee, subject) = self.place(value);
        let (scrutinee, subject) = self.through_ref(scrutinee, subject, value.span);
        let text_arms;
        let arms = match subject == Types::STR || subject == Types::STRING {
            true => {
                text_arms = self.text_arms(arms);
                &text_arms[..]
            }
            false => arms,
        };
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
        let wraps = self.wrapper(expected);
        let mut arm_ty: Option<Ty> = wraps;
        let mut first_block: Option<Span> = None;
        let mut mismatched = false;
        let mut checked = Vec::new();
        let temporary = matches!(scrutinee, Place::Temp(_));
        for (arm, shape) in arms.iter().zip(patterns) {
            let (pattern, body) = (&arm.pattern, &arm.body);
            let take = temporary && arm.guard.is_none();
            self.diverges = before;
            self.enter();
            let bindings: Vec<_> = shape
                .binds
                .into_iter()
                .map(|(name, ty, payloads)| {
                    let local = self.declare(&name, ty, false, !take, pattern.span);
                    self.fix(local, Fixed::Binding);
                    (local, payloads)
                })
                .collect();
            let guard = arm
                .guard
                .as_ref()
                .map(|guard| self.condition(guard, "A match guard must be a bool, got "));
            self.diverges = before;
            let wanted = arm_ty.or(expected);
            let mut result = self.expr(body, wanted);
            for (binding, _) in &bindings {
                self.settle_binding(*binding, &scrutinee, take);
            }
            let leaves = self.diverges;
            all_leave &= leaves;
            self.leave();
            match arm_ty {
                _ if leaves => {}
                None => {
                    arm_ty = Some(result.ty);
                    first_block =
                        matches!(body.kind, ExpressionKind::Block(_)).then_some(body.span);
                }
                Some(first) => {
                    result = match self.coerce(result, first) {
                        Ok(result) => result,
                        Err(result) => {
                            let block = matches!(body.kind, ExpressionKind::Block(_));
                            let (want, got) = (self.ty(first), self.ty(result.ty));
                            let empty = match (first == Types::VOID, result.ty == Types::VOID) {
                                (true, _) => first_block.map(|at| (at, got.clone())),
                                (_, true) if block => Some((body.span, want.clone())),
                                _ => None,
                            };
                            if self.cx.types.is_error(result.ty) {
                            } else if let Some((at, other)) = empty {
                                self.error(
                                    format!("This block gives no value, but the other arms give {other}"),
                                    at,
                                )
                                .help("a block leaves only with return, break or continue; make this arm one expression, or work the value out before the match");
                            } else {
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
                guard,
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

    /// The arms of a match on text, each value it lists tested by its guard:
    /// `"a" | "b" => x` becomes `t if t == "a" || t == "b" => x`, `t` bound
    /// to the whole text.
    fn text_arms(&mut self, arms: &[MatchArm]) -> Vec<MatchArm> {
        let mut seen: Vec<(Vec<u8>, Span)> = Vec::new();
        let mut rewritten = Vec::new();
        for arm in arms {
            let alternatives = match &arm.pattern.kind {
                PatternKind::Or(alternatives) => alternatives.iter().collect(),
                _ => vec![&arm.pattern],
            };
            let values = alternatives
                .iter()
                .map(|alternative| match &alternative.kind {
                    PatternKind::Constant(
                        value @ Expression {
                            kind: ExpressionKind::StringLit(_) | ExpressionKind::Identifier(_),
                            ..
                        },
                    ) if whole_binding(alternative).is_none_or(|name| self.names_a_value(name)) => {
                        Some(value)
                    }
                    _ => None,
                });
            let Some(values) = values.collect::<Option<Vec<_>>>() else {
                rewritten.push(arm.clone());
                continue;
            };
            let text = |span| Expression::new(ExpressionKind::Identifier(TEXT.into()), span);
            let infix = |left: Expression, operator, right: Expression| {
                let span = left.span.merge(right.span);
                let (left, right) = (Box::new(left), Box::new(right));
                Expression::new(
                    ExpressionKind::Infix {
                        left,
                        operator,
                        right,
                    },
                    span,
                )
            };
            for value in &values {
                let ExpressionKind::StringLit(bytes) = &value.kind else {
                    continue;
                };
                if let Some((_, first)) = seen.iter().find(|(seen, _)| seen == bytes) {
                    self.error(
                        "This value is already matched by an earlier arm",
                        value.span,
                    )
                    .note("matched first here, so this arm never runs for it", *first)
                    .help("remove this arm, or narrow it to values no earlier arm takes");
                } else if arm.guard.is_none() {
                    seen.push((bytes.clone(), value.span));
                }
            }
            let test = values
                .into_iter()
                .map(|value| infix(text(value.span), Token::Eq, value.clone()))
                .reduce(|left, right| infix(left, Token::Or, right))
                .expect("a pattern has an alternative");
            let guard = match &arm.guard {
                Some(guard) => infix(test, Token::And, guard.clone()),
                None => test,
            };
            rewritten.push(MatchArm {
                pattern: Pattern {
                    kind: PatternKind::Constant(text(arm.pattern.span)),
                    span: arm.pattern.span,
                },
                guard: Some(guard),
                body: arm.body.clone(),
            });
        }
        rewritten
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
    fn patterns(&mut self, subject: Ty, arms: &[MatchArm], span: Span) -> Vec<Shape> {
        let none = || {
            arms.iter()
                .map(|arm| {
                    let mut names = Vec::new();
                    first_bindings(&arm.pattern, &mut names);
                    Shape {
                        tags: Vec::new(),
                        binds: names
                            .into_iter()
                            .map(|name| (name, Types::ERROR, vec![Payload::SomeValue.into()]))
                            .collect(),
                    }
                })
                .collect()
        };
        let mut elements = Vec::new();
        let all: Option<Vec<String>> = match self.cx.types.kind(subject).clone() {
            TyKind::Error => return none(),
            TyKind::Int(_) => None,
            _ if subject == Types::STR || subject == Types::STRING => None,
            TyKind::Tuple(parts) if !parts.is_empty() => {
                elements = parts;
                None
            }
            _ => match self.listed(subject) {
                Some(all) => Some(all),
                None => {
                    let shown = self.ty(subject);
                    let head = self.cx.sources.first_line(span);
                    self.error(
                        format!(
                            "Cannot match on {shown}, only on an integer, a bool, text, an enum, a T?, a T! or a tuple of them"
                        ),
                        head,
                    )
                    .help("compare it with if and else if instead");
                    return none();
                }
            },
        };
        let listed: Vec<Vec<String>> = elements
            .iter()
            .map(|&element| {
                let element = self.pointee(element);
                self.listed(element).unwrap_or_default()
            })
            .collect();
        let combos = combinations(&listed);

        let mut covered = Vec::new();
        let mut has_default = false;
        let mut shapes = Vec::new();
        for (at, arm) in arms.iter().enumerate() {
            let pattern = &arm.pattern;
            let guarded = arm.guard.is_some();
            if matches!(pattern.kind, PatternKind::Default) {
                if at + 1 < arms.len() {
                    let after = arms[at + 1].pattern.span;
                    self.error("'default' goes last, after every other arm", pattern.span)
                        .note("this arm comes after it, so it can never be chosen", after)
                        .help("move the default arm to the end of the match");
                }
                if guarded {
                    self.error(
                        "'default' takes every other value, with no 'if'; bind a name to test it, as in x if x > 3",
                        pattern.span,
                    );
                }
                has_default = true;
            }
            if let Some(name) = whole_binding(pattern)
                && guarded
                && !self.names_a_value(name)
            {
                shapes.push(Shape {
                    tags: Vec::new(),
                    binds: vec![(name.to_string(), subject, vec![Payload::Whole.into()])],
                });
                continue;
            }
            shapes.push(self.shape(pattern, subject, guarded, &mut covered, combos.as_deref()));
        }

        if has_default {
            return shapes;
        }
        let missing: Vec<String> = match (all, &combos) {
            (Some(all), _) => (0..)
                .zip(all)
                .filter(|(at, _)| !covered.iter().any(|(tests, _)| takes(tests, &[*at])))
                .map(|(_, name)| name)
                .collect(),
            (None, Some(combos)) if !elements.is_empty() => {
                let missing = combos
                    .iter()
                    .filter(|combo| !covered.iter().any(|(tests, _)| takes(tests, combo)))
                    .map(|combo| combo.iter().map(|&tag| Some(tag)).collect())
                    .collect();
                merged(missing, &listed)
                    .into_iter()
                    .map(|combo| {
                        let parts: Vec<&str> = combo
                            .iter()
                            .zip(&listed)
                            .map(|(&tag, names)| match tag {
                                Some(OTHER) | None => "_",
                                Some(tag) => &names[tag as usize],
                            })
                            .collect();
                        format!("({})", parts.join(", "))
                    })
                    .collect()
            }
            (None, _) => {
                let shown = self.ty(subject);
                let fix = self.added_arms(arms, &["default".to_string()], span);
                let head = self.cx.sources.first_line(span);
                let error = self.error(format!("A match on {shown} needs a 'default' arm"), head);
                error.help("add a default arm for the values no other arm takes");
                if let Some((at, arms)) = fix {
                    error.fix(at, arms);
                }
                return shapes;
            }
        };
        if !missing.is_empty() {
            const SHOWN: usize = 4;
            let added = match missing.len() > SHOWN {
                true => vec!["default".to_string()],
                false => missing.clone(),
            };
            let fix = self.added_arms(arms, &added, span);
            let head = self.cx.sources.first_line(span);
            let listed = match missing.len() > SHOWN {
                true => format!(
                    "{}, and {} more",
                    missing[..SHOWN].join(", "),
                    missing.len() - SHOWN
                ),
                false => missing.join(", "),
            };
            let error = self.error(format!("This match does not cover {listed}"), head);
            error.help("add an arm for each, or a default arm");
            if let Some((at, arms)) = fix {
                error.fix(at, arms);
            }
        }
        shapes
    }

    /// What `ty` refers to when it is a reference, which a tuple element is
    /// matched through; else `ty`.
    fn pointee(&self, ty: Ty) -> Ty {
        match *self.cx.types.kind(ty) {
            TyKind::Ref(inner) | TyKind::RefMut(inner) => inner,
            _ => ty,
        }
    }

    /// The values of `ty` a match lists one by one, written as patterns:
    /// none for an integer, text, or anything a match cannot list.
    fn listed(&mut self, ty: Ty) -> Option<Vec<String>> {
        Some(match self.cx.types.kind(ty) {
            TyKind::Bool => vec!["false".into(), "true".into()],
            TyKind::Optional(_) => vec!["None".into(), "Some(_)".into()],
            TyKind::Result(..) => vec!["Err(_)".into(), "Ok(_)".into()],
            &TyKind::Adt(def, _) if self.cx.is_enum(ty).is_some() => {
                let name = self.cx.items.item(def).name.clone();
                self.cx
                    .variants_of(def)
                    .iter()
                    .map(|(variant, fields)| match fields.len() {
                        0 => format!("{name}::{variant}"),
                        count => format!("{name}::{variant}({})", vec!["_"; count].join(", ")),
                    })
                    .collect()
            }
            _ => return None,
        })
    }

    /// The arms for `patterns`, each with an empty body, and where they go
    /// in the match written at `span`: on lines of their own before its
    /// closing brace, indented as its other arms.
    fn added_arms(
        &self,
        arms: &[MatchArm],
        patterns: &[String],
        span: Span,
    ) -> Option<(Span, String)> {
        let sources = self.cx.sources;
        let text = sources.text(span);
        let close = text.rfind('}')?;
        let line = text[..close].rfind('\n')? + 1;
        if !text[line..close].trim().is_empty() {
            return None;
        }
        let indent = match arms.last() {
            Some(arm) => {
                let before = sources.text(Span::new(span.start, arm.pattern.span.start));
                let start = before.rfind('\n').map_or(0, |at| at + 1);
                before[start..].to_string()
            }
            None => format!("{}    ", &text[line..close]),
        };
        let bare = arms.last().filter(|arm| {
            let after = Span::new(arm.body.span.end, span.start + close);
            !matches!(arm.body.kind, ExpressionKind::Block(_)) && !sources.text(after).contains(',')
        });
        if let Some(arm) = bare {
            let added: String = patterns
                .iter()
                .map(|pattern| format!(",\n{indent}{pattern} => {{}}"))
                .collect();
            let at = arm.body.span.end;
            return Some((Span::new(at, at), added));
        }
        let added: String = patterns
            .iter()
            .map(|pattern| format!("{indent}{pattern} => {{}}\n"))
            .collect();
        let at = span.start + line;
        Some((Span::new(at, at), added))
    }

    /// An arm's pattern: each alternative takes values no earlier one took,
    /// and all bind the same names at the same types. The values of an arm
    /// with a guard may still go on to later arms, so they stay uncovered.
    /// A match on a tuple checks the `combos` of its elements' tags.
    fn shape(
        &mut self,
        pattern: &Pattern,
        subject: Ty,
        guarded: bool,
        covered: &mut Vec<(Vec<Test>, Span)>,
        combos: Option<&[Vec<i128>]>,
    ) -> Shape {
        let alternatives = match &pattern.kind {
            PatternKind::Default => &[],
            PatternKind::Or(alternatives) => alternatives.as_slice(),
            _ => std::slice::from_ref(pattern),
        };
        let mut shape: Option<Shape> = None;
        for alternative in alternatives {
            for (tests, binds) in self.tested(alternative, subject) {
                if let Some(first) = repeated(&tests, covered, combos) {
                    self.error(
                        "This value is already matched by an earlier arm",
                        alternative.span,
                    )
                    .note("matched first here, so this arm never runs for it", first)
                    .help("remove this arm, or narrow it to values no earlier arm takes");
                }
                if !guarded {
                    covered.push((tests.clone(), alternative.span));
                }
                let Some(shape) = &mut shape else {
                    shape = Some(Shape {
                        tags: vec![tests],
                        binds: binds
                            .into_iter()
                            .map(|(name, ty, reach)| (name, ty, vec![reach]))
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
                for (name, ty, reach) in binds {
                    let (_, first, reaches) = shape
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
                    reaches.push(reach);
                }
                shape.tags.push(tests);
            }
        }
        shape.unwrap_or_default()
    }

    /// The alternatives a pattern with no `|` at its top makes: one, or for
    /// a tuple one per way of picking an alternative of each element.
    fn tested(&mut self, pattern: &Pattern, subject: Ty) -> Vec<Tested> {
        let shown = self.ty(subject);
        let TyKind::Tuple(elements) = self.cx.types.kind(subject).clone() else {
            if matches!(pattern.kind, PatternKind::Tuple(_)) {
                self.error(format!("This pattern cannot match {shown}"), pattern.span);
                return Vec::new();
            }
            let Some(((low, high), binds)) = self.pattern(pattern, subject) else {
                return Vec::new();
            };
            let test = Test {
                slot: None,
                low,
                high,
            };
            let binds = binds
                .into_iter()
                .map(|(name, ty, payload)| (name, ty, payload.into()))
                .collect();
            return vec![(vec![test], binds)];
        };
        let PatternKind::Tuple(parts) = &pattern.kind else {
            self.error(format!("This pattern cannot match {shown}"), pattern.span);
            return Vec::new();
        };
        if parts.len() != elements.len() {
            self.error(
                format!(
                    "This pattern has {} parts, but {shown} has {}",
                    parts.len(),
                    elements.len()
                ),
                pattern.span,
            );
            return Vec::new();
        }
        let mut product: Vec<Tested> = vec![(Vec::new(), Vec::new())];
        for (slot, (part, &element)) in (0..).zip(parts.iter().zip(&elements)) {
            let Some(options) = self.tuple_part(part, element, slot) else {
                return Vec::new();
            };
            product = product
                .into_iter()
                .flat_map(|(tests, binds)| {
                    options.iter().map(move |(test, more)| {
                        let mut tests = tests.clone();
                        tests.extend(*test);
                        let mut binds = binds.clone();
                        binds.extend(more.iter().cloned());
                        (tests, binds)
                    })
                })
                .collect();
        }
        product
    }

    /// The ways element `slot` of a tuple pattern takes a value of `ty`, or
    /// of what it refers to: a test, or none for a name or `_`, and what
    /// each binds.
    fn tuple_part(
        &mut self,
        part: &Pattern,
        ty: Ty,
        slot: u32,
    ) -> Option<Vec<(Option<Test>, Binds)>> {
        let alternatives = match &part.kind {
            PatternKind::Or(alternatives) => alternatives.iter().collect(),
            _ => vec![part],
        };
        let mut options = Vec::new();
        for alternative in alternatives {
            let whole = Reach {
                slot: Some(slot),
                payload: Payload::Whole,
            };
            let constant;
            let tested = match &alternative.kind {
                PatternKind::Binding(name) if name == "_" => {
                    options.push((None, Vec::new()));
                    continue;
                }
                PatternKind::Binding(name) if !self.names_a_value(name) => {
                    options.push((None, vec![(name.clone(), ty, whole)]));
                    continue;
                }
                PatternKind::Tuple(_) => {
                    self.error(
                        "A tuple inside a tuple pattern is not taken apart; bind it to a name and match that inside the arm",
                        alternative.span,
                    );
                    return None;
                }
                PatternKind::Binding(name) => {
                    let value =
                        Expression::new(ExpressionKind::Identifier(name.clone()), alternative.span);
                    constant = Pattern {
                        kind: PatternKind::Constant(value),
                        span: alternative.span,
                    };
                    &constant
                }
                _ => alternative,
            };
            let ty = self.pointee(ty);
            if self.listed(ty).is_none() && self.cx.types.as_int(ty).is_none() {
                let shown = self.ty(ty);
                self.error(
                    format!("A part of type {shown} cannot be matched; bind it to a name and compare it inside the arm"),
                    alternative.span,
                );
                return None;
            }
            let ((low, high), binds) = self.pattern(tested, ty)?;
            let test = Test {
                slot: Some(slot),
                low,
                high,
            };
            let binds = binds
                .into_iter()
                .map(|(name, ty, payload)| {
                    (
                        name,
                        ty,
                        Reach {
                            slot: Some(slot),
                            payload,
                        },
                    )
                })
                .collect();
            options.push((Some(test), binds));
        }
        Some(options)
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
            }) if matches!(subject_kind, TyKind::Optional(_)) => return Some(((0, 0), Vec::new())),
            PatternKind::Constant(value) => {
                if let Some(name) = whole_binding(pattern)
                    && self.local(name).is_none()
                    && self
                        .cx
                        .items
                        .lookup_value(self.scope.module, name)
                        .is_none()
                {
                    self.error(
                        format!("'{name}' names no value; a name alone matches only with an 'if' after it, as in {name} if {name} > 3, and 'default' takes every other value"),
                        pattern.span,
                    );
                    return None;
                }
                let tag = self.value_pattern(value, subject)?;
                return Some(((tag, tag), Vec::new()));
            }
            PatternKind::Range(low, high) => return self.range_pattern(low, high, subject),
            PatternKind::Binding(_)
            | PatternKind::Or(_)
            | PatternKind::Default
            | PatternKind::Tuple(_) => {
                unreachable!("the parser puts these only in a payload or as a whole arm")
            }
        };
        let found = match (path.as_str(), &subject_kind) {
            ("Some", TyKind::Optional(inner)) => {
                Some((1, vec![*inner], (|_, _| Payload::SomeValue) as PayloadAt))
            }
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
        Some(((tag, tag), binds))
    }

    /// `low..=high`, the integers from one to the other, which must not be
    /// fewer than one.
    fn range_pattern(
        &mut self,
        low: &Expression,
        high: &Expression,
        subject: Ty,
    ) -> Option<Alternative> {
        if self.cx.types.as_int(subject).is_none() {
            let shown = self.ty(subject);
            self.error(
                format!("A range matches integers, not {shown}"),
                low.span.merge(high.span),
            );
            return None;
        }
        let (low_value, high_value) = (
            self.value_pattern(low, subject)?,
            self.value_pattern(high, subject)?,
        );
        if low_value > high_value {
            self.error(
                format!("The range {low_value}..={high_value} takes no value"),
                low.span.merge(high.span),
            );
            return None;
        }
        Some(((low_value, high_value), Vec::new()))
    }

    /// A pattern standing for one value, which it is checked and evaluated to.
    fn value_pattern(&mut self, pattern: &Expression, subject: Ty) -> Option<i128> {
        let errors = self.cx.errors.len();
        let value = self.expr(pattern, Some(subject));
        if self.cx.types.is_error(value.ty) || self.cx.errors.len() > errors {
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
        if tag.is_none() {
            self.error("A pattern must be known at compile time", pattern.span);
        }
        tag
    }
}

/// The name a pattern is when it is a name alone, which with a guard binds
/// the whole value.
fn whole_binding(pattern: &Pattern) -> Option<&str> {
    match &pattern.kind {
        PatternKind::Constant(Expression {
            kind: ExpressionKind::Identifier(name),
            ..
        }) if !name.contains("::") => Some(name),
        _ => None,
    }
}

/// The names the first alternative of `pattern` binds.
fn first_bindings(pattern: &Pattern, names: &mut Vec<String>) {
    match &pattern.kind {
        PatternKind::Binding(name) if name != "_" => names.push(name.clone()),
        PatternKind::Payload { fields, .. } | PatternKind::Tuple(fields) => {
            fields.iter().for_each(|field| first_bindings(field, names));
        }
        PatternKind::Or(alternatives) => first_bindings(&alternatives[0], names),
        _ => {}
    }
}

/// Whether a value whose tags are `combo`, one per tuple element or the
/// value's own, passes every test.
fn takes(tests: &[Test], combo: &[i128]) -> bool {
    tests.iter().all(|test| {
        let tag = combo[test.slot.unwrap_or(0) as usize];
        tag != OTHER && test.low <= tag && tag <= test.high
    })
}

/// Every combination of tags of a tuple whose elements list `listed`, an
/// element listing nothing standing at `OTHER`; none past
/// `MAX_COMBINATIONS`.
fn combinations(listed: &[Vec<String>]) -> Option<Vec<Vec<i128>>> {
    let mut combos = vec![Vec::new()];
    for names in listed {
        let tags: Vec<i128> = match names.len() {
            0 => vec![OTHER],
            count => (0..count as i128).collect(),
        };
        if combos.len() * tags.len() > MAX_COMBINATIONS {
            return None;
        }
        combos = combos
            .into_iter()
            .flat_map(|combo| {
                tags.iter().map(move |&tag| {
                    let mut combo = combo.clone();
                    combo.push(tag);
                    combo
                })
            })
            .collect();
    }
    Some(combos)
}

/// Where an earlier arm already took every value `tests` takes: for one
/// value, any it shares; for a tuple, each combination of its `combos`.
fn repeated(
    tests: &[Test],
    covered: &[(Vec<Test>, Span)],
    combos: Option<&[Vec<i128>]>,
) -> Option<Span> {
    if let [
        Test {
            slot: None,
            low,
            high,
        },
    ] = tests
    {
        return covered
            .iter()
            .find_map(|(earlier, span)| match earlier[..] {
                [
                    Test {
                        low: first,
                        high: last,
                        ..
                    },
                ] if first <= *high && *low <= last => Some(*span),
                _ => None,
            });
    }
    let mine: Vec<&Vec<i128>> = combos?.iter().filter(|combo| takes(tests, combo)).collect();
    let mut first = None;
    for combo in mine {
        let (_, span) = covered.iter().find(|(earlier, _)| takes(earlier, combo))?;
        first = Some(*span);
    }
    first
}

/// `missing` combinations of tags, those that together take every value
/// of an element and agree on the rest written once, that element as `_`.
fn merged(mut missing: Vec<Vec<Option<i128>>>, listed: &[Vec<String>]) -> Vec<Vec<Option<i128>>> {
    for (slot, names) in listed.iter().enumerate() {
        let without = |combo: &Vec<Option<i128>>| {
            let mut rest = combo.clone();
            rest[slot] = None;
            rest
        };
        let mut kept = Vec::new();
        for combo in &missing {
            let rest = without(combo);
            let group = missing
                .iter()
                .filter(|other| without(other) == rest)
                .count();
            let entry = match group == names.len().max(1) {
                true => rest,
                false => combo.clone(),
            };
            if !kept.contains(&entry) {
                kept.push(entry);
            }
        }
        missing = kept;
    }
    missing
}
