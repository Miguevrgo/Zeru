//! Drop and deep-copy glue: one function per type, emitted the first time it
//! is needed and declared before its body, so a type reaching itself through
//! a Vec calls the function being emitted.

use inkwell::{
    IntPredicate,
    module::Linkage,
    values::{FunctionValue, PointerValue},
};

use crate::ty::{Ty, TyKind};

use super::Lower;

impl<'ctx> Lower<'_, 'ctx> {
    pub(super) fn call_drop(&mut self, value: PointerValue<'ctx>, ty: Ty) {
        let function = self.glue("drop", ty, 1, Self::emit_drop);
        self.builder
            .build_call(function, &[value.into()], "")
            .unwrap();
    }

    pub(super) fn call_copy(&mut self, dst: PointerValue<'ctx>, src: PointerValue<'ctx>, ty: Ty) {
        let function = self.glue("copy", ty, 2, Self::emit_copy);
        self.builder
            .build_call(function, &[dst.into(), src.into()], "")
            .unwrap();
    }

    fn glue(
        &mut self,
        what: &str,
        ty: Ty,
        pointers: usize,
        emit: fn(&mut Self, &[PointerValue<'ctx>], Ty),
    ) -> FunctionValue<'ctx> {
        let name = format!("{what}.{}", self.types().show(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let params = vec![self.ptr_type().into(); pointers];
        let fn_type = self.context.void_type().fn_type(&params, false);
        let function = self
            .module
            .add_function(&name, fn_type, Some(Linkage::Internal));
        self.in_helper(function, |this| {
            let args: Vec<PointerValue> = function
                .get_param_iter()
                .map(|param| param.into_pointer_value())
                .collect();
            emit(this, &args, ty);
            this.builder.build_return(None).unwrap();
        });
        function
    }

    /// Free what the value at `args[0]` owns: a struct's own `drop` first,
    /// then each part that owns something.
    fn emit_drop(&mut self, args: &[PointerValue<'ctx>], ty: Ty) {
        let value = args[0];
        match self.types().kind(ty).clone() {
            TyKind::Vec(elem) => {
                let data = self.load_field(value, ty, 0).into_pointer_value();
                if self.program.needs_drop(elem) {
                    let len = self.load_field(value, ty, 1).into_int_value();
                    self.counted_loop(len, |this, at| {
                        let item = this.buffer_slot(data, elem, at);
                        this.call_drop(item, elem);
                    });
                }
                let cap = self.load_field(value, ty, 2).into_int_value();
                self.free_buffer(data, elem, cap);
            }
            TyKind::Array(elem, len) => {
                self.each_element(&[value], ty, elem, len, |this, parts| {
                    this.call_drop(parts[0], elem)
                })
            }
            TyKind::Optional(inner) | TyKind::Result(inner, _) => {
                self.if_tag_set(value, ty, |this| {
                    let payload = this.field_ptr(value, ty, 1);
                    if this.program.needs_drop(inner) {
                        this.call_drop(payload, inner);
                    }
                });
            }
            TyKind::Adt(..) if self.program.enums.contains_key(&ty) => {
                self.each_variant(&[value], ty, |this, parts, field| {
                    this.call_drop(parts[0], field)
                })
            }
            TyKind::Adt(..) | TyKind::Tuple(_) => {
                if let Some(user) = self.program.structs.get(&ty).and_then(|s| s.drop) {
                    let user = self.functions[user.0 as usize];
                    self.builder.build_call(user, &[value.into()], "").unwrap();
                }
                self.each_part(&[value], ty, |this, parts, part| {
                    this.call_drop(parts[0], part)
                });
            }
            _ => {}
        }
    }

    /// Make `args[0]` a copy of `args[1]`: every byte, then a buffer of its
    /// own for each part that owns one.
    fn emit_copy(&mut self, args: &[PointerValue<'ctx>], ty: Ty) {
        let (dst, src) = (args[0], args[1]);
        let size = self.size_of(ty);
        self.builder.build_memcpy(dst, 1, src, 1, size).unwrap();
        match self.types().kind(ty).clone() {
            TyKind::Vec(elem) => {
                let data = self.load_field(src, ty, 0).into_pointer_value();
                let len = self.load_field(src, ty, 1).into_int_value();
                let fresh = self.alloc_buffer(elem, len);
                if self.program.needs_drop(elem) {
                    self.counted_loop(len, |this, at| {
                        let to = this.buffer_slot(fresh, elem, at);
                        let from = this.buffer_slot(data, elem, at);
                        this.call_copy(to, from, elem);
                    });
                } else {
                    self.move_elements(fresh, data, elem, len);
                }
                let ptr_field = self.field_ptr(dst, ty, 0);
                self.builder.build_store(ptr_field, fresh).unwrap();
                let cap_field = self.field_ptr(dst, ty, 2);
                self.builder.build_store(cap_field, len).unwrap();
            }
            TyKind::Array(elem, len) => {
                self.each_element(&[dst, src], ty, elem, len, |this, parts| {
                    this.call_copy(parts[0], parts[1], elem)
                })
            }
            TyKind::Optional(inner) | TyKind::Result(inner, _) => {
                self.if_tag_set(src, ty, |this| {
                    if this.program.needs_drop(inner) {
                        let to = this.field_ptr(dst, ty, 1);
                        let from = this.field_ptr(src, ty, 1);
                        this.call_copy(to, from, inner);
                    }
                });
            }
            TyKind::Adt(..) if self.program.enums.contains_key(&ty) => {
                self.each_variant(&[dst, src], ty, |this, parts, field| {
                    this.call_copy(parts[0], parts[1], field)
                })
            }
            TyKind::Adt(..) | TyKind::Tuple(_) => {
                self.each_part(&[dst, src], ty, |this, parts, part| {
                    this.call_copy(parts[0], parts[1], part)
                });
            }
            _ => {}
        }
    }

    /// Run `visit` on the same field of each struct or tuple in `values`, for
    /// each field that owns something.
    fn each_part(
        &mut self,
        values: &[PointerValue<'ctx>],
        ty: Ty,
        mut visit: impl FnMut(&mut Self, &[PointerValue<'ctx>], Ty),
    ) {
        let parts = match self.types().kind(ty) {
            TyKind::Tuple(parts) => parts.clone(),
            _ => self.program.structs[&ty].fields.clone(),
        };
        for (at, part) in parts.into_iter().enumerate() {
            if !self.program.needs_drop(part) {
                continue;
            }
            let fields: Vec<_> = values
                .iter()
                .map(|&value| self.field_ptr(value, ty, at as u32))
                .collect();
            visit(self, &fields, part);
        }
    }

    /// Run `visit` on each value the variant held by the enum at `values[0]`
    /// carries, for each variant whose values own something.
    fn each_variant(
        &mut self,
        values: &[PointerValue<'ctx>],
        ty: Ty,
        mut visit: impl FnMut(&mut Self, &[PointerValue<'ctx>], Ty),
    ) {
        if self.is_scalar(ty) {
            return;
        }
        let tag = self.load_field(values[0], ty, 0).into_int_value();
        let variants = self.program.enums[&ty].variants.clone();
        for (index, fields) in variants.iter().enumerate() {
            if !fields.iter().any(|&f| self.program.needs_drop(f)) {
                continue;
            }
            let holds = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    tag,
                    self.context.i32_type().const_int(index as u64, false),
                    "holds",
                )
                .unwrap();
            self.if_then(holds, |this| {
                for (at, &field) in fields.iter().enumerate() {
                    if !this.program.needs_drop(field) {
                        continue;
                    }
                    let parts: Vec<_> = values
                        .iter()
                        .map(|&value| this.variant_field_ptr(value, ty, index as u32, at as u32))
                        .collect();
                    visit(this, &parts, field);
                }
            });
        }
    }

    fn each_element(
        &mut self,
        values: &[PointerValue<'ctx>],
        ty: Ty,
        elem: Ty,
        len: u64,
        mut visit: impl FnMut(&mut Self, &[PointerValue<'ctx>]),
    ) {
        if !self.program.needs_drop(elem) {
            return;
        }
        let count = self.usize_type().const_int(len, false);
        self.counted_loop(count, |this, at| {
            let elements: Vec<_> = values
                .iter()
                .map(|&value| this.element_ptr(value, ty, at))
                .collect();
            visit(this, &elements);
        });
    }

    fn if_tag_set(&mut self, value: PointerValue<'ctx>, ty: Ty, then: impl FnOnce(&mut Self)) {
        let tag = self.load_field(value, ty, 0).into_int_value();
        self.if_then(tag, then);
    }
}
