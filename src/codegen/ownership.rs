//! Ownership at runtime. A value that owns memory has a drop flag, raised
//! while it still owns it and lowered when it is moved away, and is dropped as
//! its scope closes. Dropping and deep-copying go through glue functions, one
//! per type, emitted the first time each is needed.

use inkwell::{
    IntPredicate,
    module::Linkage,
    types::{BasicType, StructType},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::{
    codegen::{
        compiler::{Compiler, Owned},
        layout::{VEC_CAP, VEC_LEN, VEC_PTR},
    },
    sema::types::Type,
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Make the innermost scope own the value in `slot`, if `ty` owns memory.
    pub(super) fn own(&mut self, slot: PointerValue<'ctx>, ty: &Type) {
        if let Some(owned) = self.raise_flag(slot, ty)
            && let Some(scope) = self.scope_stack.last_mut()
        {
            scope.owned.push(owned);
        }
    }

    /// Give `value`, which nothing owns, a slot of its own. If it owns memory
    /// it is dropped once its statement is done.
    pub(super) fn adopt_temporary(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: Option<&Type>,
    ) -> PointerValue<'ctx> {
        let function = self.current_fn.expect("a temporary lives in a function");
        let slot = self.create_entry_block_alloca(function, "temp", value.get_type());
        self.builder.build_store(slot, value).unwrap();
        if let Some(owned) = ty.and_then(|ty| self.raise_flag(slot, ty)) {
            self.temporaries.push(owned);
        }
        slot
    }

    /// A drop flag for `slot`, raised here. It starts lowered in the entry
    /// block, so a drop on a path that never got here does nothing.
    fn raise_flag(&mut self, slot: PointerValue<'ctx>, ty: &Type) -> Option<Owned<'ctx>> {
        if !self.types.owns_heap(ty) {
            return None;
        }
        let flag = self.create_entry_flag(self.current_fn?);
        let raised = self.context.bool_type().const_int(1, false);
        self.builder.build_store(flag, raised).unwrap();
        Some(Owned {
            slot,
            flag,
            ty: ty.clone(),
        })
    }

    pub(super) fn owned_at(&self, slot: PointerValue<'ctx>) -> Option<Owned<'ctx>> {
        self.scope_stack
            .iter()
            .flat_map(|scope| &scope.owned)
            .find(|owned| owned.slot == slot)
            .cloned()
    }

    /// The variable at `slot` was given away: it no longer drops its value.
    pub(super) fn release(&mut self, slot: PointerValue<'ctx>) {
        if let Some(owned) = self.owned_at(slot) {
            let lowered = self.context.bool_type().const_zero();
            self.builder.build_store(owned.flag, lowered).unwrap();
        }
    }

    /// Drop what scopes `depth..` own, innermost and latest first, as a
    /// `return`, a `break` or the end of a block leaves them.
    pub(super) fn drop_scopes_from(&mut self, depth: usize) {
        let owned: Vec<Owned<'ctx>> = self.scope_stack[depth..]
            .iter()
            .rev()
            .flat_map(|scope| scope.owned.iter().rev().cloned())
            .collect();
        for owned in &owned {
            self.drop_owned(owned);
        }
    }

    /// Drop everything the function owns, as a `return` leaves it. The
    /// temporaries stay listed: a `try` returns on one path only.
    pub(super) fn drop_all_owned(&mut self) {
        for owned in self.temporaries.clone().iter().rev() {
            self.drop_owned(owned);
        }
        self.drop_scopes_from(0);
    }

    pub(super) fn drop_temporaries(&mut self) {
        for owned in std::mem::take(&mut self.temporaries).iter().rev() {
            self.drop_owned(owned);
        }
    }

    pub(super) fn drop_owned(&mut self, owned: &Owned<'ctx>) {
        if !self.block_is_open() {
            return;
        }
        let bool_type = self.context.bool_type();
        let live = self.load_int(bool_type, owned.flag, "owns");
        self.if_then(live, |this| {
            this.call_drop(owned.slot, &owned.ty);
            this.builder
                .build_store(owned.flag, bool_type.const_zero())
                .unwrap();
        });
    }

    pub(super) fn copy_value(
        &mut self,
        src: PointerValue<'ctx>,
        ty: &Type,
    ) -> Option<BasicValueEnum<'ctx>> {
        let llvm_type = self.llvm_type_of(ty)?;
        let dst = self.create_entry_block_alloca(self.current_fn?, "copy", llvm_type);
        self.call_copy(dst, src, ty);
        Some(self.load(llvm_type, dst, "copy"))
    }

    pub(super) fn call_drop(&mut self, value: PointerValue<'ctx>, ty: &Type) {
        let drop_fn = self.glue_fn("drop", ty, 1, Self::emit_drop);
        self.builder
            .build_call(drop_fn, &[value.into()], "")
            .unwrap();
    }

    fn call_copy(&mut self, dst: PointerValue<'ctx>, src: PointerValue<'ctx>, ty: &Type) {
        let copy_fn = self.glue_fn("copy", ty, 2, Self::emit_copy);
        self.builder
            .build_call(copy_fn, &[dst.into(), src.into()], "")
            .unwrap();
    }

    /// `void <what>.<ty>(ptr...)`, its body emitted by `emit` the first time.
    /// It is declared before the body, so a type reaching itself through a
    /// Vec calls the function being emitted.
    fn glue_fn(
        &mut self,
        what: &str,
        ty: &Type,
        pointers: usize,
        emit: fn(&mut Self, &[PointerValue<'ctx>], &Type),
    ) -> FunctionValue<'ctx> {
        let name = format!("{what}.{ty}");
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
    fn emit_drop(&mut self, args: &[PointerValue<'ctx>], ty: &Type) {
        let value = args[0];
        match ty {
            Type::Vec { elem_type } => {
                let Some(elem) = self.llvm_type_of(elem_type) else {
                    return;
                };
                let data = self.load_vec_field(value, VEC_PTR).into_pointer_value();
                if self.types.owns_heap(elem_type) {
                    let len = self.load_vec_field(value, VEC_LEN).into_int_value();
                    self.build_counted_loop(len, |this, at| {
                        let item = this.vec_elem_ptr(data, at, elem);
                        this.call_drop(item, elem_type);
                    });
                }
                let cap = self.load_vec_field(value, VEC_CAP).into_int_value();
                self.free_buffer(data, elem, cap);
            }
            Type::Array { elem_type, len } => {
                self.each_element(value, None, ty, *len, |this, item, _| {
                    this.call_drop(item, elem_type)
                });
            }
            Type::Optional(inner) | Type::Result { ok_type: inner, .. } => {
                let shape = self.shape_of(ty);
                self.if_tag_set(value, shape, |this| {
                    let payload = [(1, inner.as_ref().clone())];
                    this.each_part(shape, &[value], &payload, |this, parts, ty| {
                        this.call_drop(parts[0], ty)
                    })
                });
            }
            Type::Enum(_) => {
                self.each_variant(&[value], ty, |this, parts, ty| this.call_drop(parts[0], ty))
            }
            Type::Struct(_) | Type::Tuple(_) => {
                if let Type::Struct(name) = ty
                    && let Some(user_drop) = self.module.get_function(&format!("{name}::drop"))
                {
                    self.builder
                        .build_call(user_drop, &[value.into()], "")
                        .unwrap();
                }
                let shape = self.shape_of(ty);
                let parts = self.parts_of(ty);
                self.each_part(shape, &[value], &parts, |this, parts, ty| {
                    this.call_drop(parts[0], ty)
                });
            }
            _ => {}
        }
    }

    /// Make `args[0]` a copy of `args[1]`: every byte, then a buffer of its
    /// own for each part that owns one.
    fn emit_copy(&mut self, args: &[PointerValue<'ctx>], ty: &Type) {
        let (dst, src) = (args[0], args[1]);
        let Some(llvm_type) = self.llvm_type_of(ty) else {
            return;
        };
        let bits = self.load(llvm_type, src, "bits");
        self.builder.build_store(dst, bits).unwrap();

        match ty {
            Type::Vec { elem_type } => {
                let Some(elem) = self.llvm_type_of(elem_type) else {
                    return;
                };
                let data = self.load_vec_field(src, VEC_PTR).into_pointer_value();
                let len = self.load_vec_field(src, VEC_LEN).into_int_value();
                let fresh = self.alloc_buffer(elem, len);
                if self.types.owns_heap(elem_type) {
                    self.build_counted_loop(len, |this, at| {
                        let to = this.vec_elem_ptr(fresh, at, elem);
                        let from = this.vec_elem_ptr(data, at, elem);
                        this.call_copy(to, from, elem_type);
                    });
                } else {
                    self.move_bytes(fresh, data, elem, len);
                }
                let ptr_field = self.vec_field_ptr(dst, VEC_PTR, "ptr_field");
                self.builder.build_store(ptr_field, fresh).unwrap();
                let cap_field = self.vec_field_ptr(dst, VEC_CAP, "cap_field");
                self.builder.build_store(cap_field, len).unwrap();
            }
            Type::Array { elem_type, len } => {
                self.each_element(dst, Some(src), ty, *len, |this, to, from| {
                    this.call_copy(to, from.unwrap(), elem_type)
                });
            }
            Type::Optional(inner) | Type::Result { ok_type: inner, .. } => {
                let shape = self.shape_of(ty);
                self.if_tag_set(src, shape, |this| {
                    let payload = [(1, inner.as_ref().clone())];
                    this.each_part(shape, &[dst, src], &payload, |this, parts, ty| {
                        this.call_copy(parts[0], parts[1], ty)
                    })
                });
            }
            Type::Enum(_) => self.each_variant(&[dst, src], ty, |this, parts, ty| {
                this.call_copy(parts[0], parts[1], ty)
            }),
            Type::Struct(_) | Type::Tuple(_) => {
                let shape = self.shape_of(ty);
                let parts = self.parts_of(ty);
                self.each_part(shape, &[dst, src], &parts, |this, parts, ty| {
                    this.call_copy(parts[0], parts[1], ty)
                });
            }
            _ => {}
        }
    }

    /// Run `visit` on the values of the variant the enum at `values[0]`
    /// holds, for each variant whose values own something.
    fn each_variant(
        &mut self,
        values: &[PointerValue<'ctx>],
        ty: &Type,
        mut visit: impl FnMut(&mut Self, &[PointerValue<'ctx>], &Type),
    ) {
        let (Type::Enum(name), Some(shape)) = (ty, self.shape_of(ty)) else {
            return;
        };
        let types = self.types;
        let i32_type = self.context.i32_type();
        let tag_field = self
            .builder
            .build_struct_gep(shape, values[0], 0, "tag")
            .unwrap();
        let tag = self.load_int(i32_type, tag_field, "tag");
        for (at, (_, fields)) in (0..).zip(types.enum_variants(name).unwrap_or_default()) {
            if !fields.iter().any(|field| types.owns_heap(field)) {
                continue;
            }
            let holds = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    tag,
                    i32_type.const_int(at, false),
                    "holds",
                )
                .unwrap();
            let payload = self.payload_type(fields);
            let parts: Vec<(u32, Type)> = (0..).zip(fields.iter().cloned()).collect();
            self.if_then(holds, |this| {
                let areas: Vec<_> = values
                    .iter()
                    .map(|value| {
                        this.builder
                            .build_struct_gep(shape, *value, 1, "payload")
                            .unwrap()
                    })
                    .collect();
                this.each_part(payload, &areas, &parts, &mut visit);
            });
        }
    }

    fn parts_of(&self, ty: &Type) -> Vec<(u32, Type)> {
        let types: Vec<Type> = match ty {
            Type::Struct(name) => self
                .types
                .struct_fields(name)
                .iter()
                .map(|(_, ty)| ty.clone())
                .collect(),
            Type::Tuple(parts) => parts.clone(),
            _ => Vec::new(),
        };
        (0..).zip(types).collect()
    }

    fn shape_of(&self, ty: &Type) -> Option<StructType<'ctx>> {
        self.llvm_type_of(ty).map(|ty| ty.into_struct_type())
    }

    /// Run `visit` on the fields `parts` of each aggregate in `values` that own
    /// something, handing it the same field of every one of them.
    fn each_part(
        &mut self,
        shape: Option<StructType<'ctx>>,
        values: &[PointerValue<'ctx>],
        parts: &[(u32, Type)],
        mut visit: impl FnMut(&mut Self, &[PointerValue<'ctx>], &Type),
    ) {
        let Some(shape) = shape else {
            return;
        };
        for (at, ty) in parts {
            if !self.types.owns_heap(ty) {
                continue;
            }
            let fields: Vec<_> = values
                .iter()
                .map(|value| {
                    self.builder
                        .build_struct_gep(shape, *value, *at, "part")
                        .unwrap()
                })
                .collect();
            visit(self, &fields, ty);
        }
    }

    /// Run `visit` on each element of the array at `value` (and `other`), when
    /// its elements own something.
    fn each_element(
        &mut self,
        value: PointerValue<'ctx>,
        other: Option<PointerValue<'ctx>>,
        ty: &Type,
        len: usize,
        mut visit: impl FnMut(&mut Self, PointerValue<'ctx>, Option<PointerValue<'ctx>>),
    ) {
        let Type::Array { elem_type, .. } = ty else {
            return;
        };
        let Some(array) = self.llvm_type_of(ty) else {
            return;
        };
        if !self.types.owns_heap(elem_type) {
            return;
        }
        let usize_type = self.usize_type();
        let count = usize_type.const_int(len as u64, false);
        self.build_counted_loop(count, |this, at| {
            let element = |this: &Self, base: PointerValue<'ctx>| unsafe {
                this.builder
                    .build_in_bounds_gep(array, base, &[usize_type.const_zero(), at], "element")
                    .unwrap()
            };
            let first = element(this, value);
            let second = other.map(|base| element(this, base));
            visit(this, first, second);
        });
    }

    fn if_tag_set(
        &mut self,
        value: PointerValue<'ctx>,
        shape: Option<StructType<'ctx>>,
        then: impl FnOnce(&mut Self),
    ) {
        let Some(shape) = shape else {
            return;
        };
        let tag_field = self
            .builder
            .build_struct_gep(shape, value, 0, "tag")
            .unwrap();
        let tag = self.load_int(self.context.bool_type(), tag_field, "tag");
        self.if_then(tag, then);
    }

    pub(super) fn if_then(&mut self, condition: IntValue<'ctx>, then: impl FnOnce(&mut Self)) {
        let Some(function) = self.current_fn else {
            return;
        };
        let then_bb = self.context.append_basic_block(function, "then");
        let done_bb = self.context.append_basic_block(function, "done");
        self.builder
            .build_conditional_branch(condition, then_bb, done_bb)
            .unwrap();
        self.builder.position_at_end(then_bb);
        then(self);
        self.builder.build_unconditional_branch(done_bb).unwrap();
        self.builder.position_at_end(done_bb);
    }

    pub(super) fn block_is_open(&self) -> bool {
        self.builder
            .get_insert_block()
            .is_some_and(|block| block.get_terminator().is_none())
    }

    fn load_vec_field(&self, vec: PointerValue<'ctx>, field: u32) -> BasicValueEnum<'ctx> {
        let field_ptr = self.vec_field_ptr(vec, field, "field");
        let field_type = self.vec_type().get_field_type_at_index(field).unwrap();
        self.load(field_type.as_basic_type_enum(), field_ptr, "field")
    }
}
