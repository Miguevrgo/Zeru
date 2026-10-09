//! Line tables for debug builds: a subprogram per function and a location per
//! statement, so a debugger can break on a line and step through the source.

use std::{collections::HashMap, path::Path};

use inkwell::{
    debug_info::{
        AsDIScope, DIFile, DIFlags, DIFlagsConstants, DISubprogram, DWARFEmissionKind,
        DWARFSourceLanguage, DebugInfoBuilder,
    },
    module::FlagBehavior,
    values::FunctionValue,
};

use crate::{
    codegen::{SafetyMode, compiler::Compiler},
    errors::Span,
};

pub(super) struct Debug<'ctx> {
    builder: DebugInfoBuilder<'ctx>,
    files: HashMap<String, DIFile<'ctx>>,
    /// The function being lowered, which every location belongs to.
    scope: Option<DISubprogram<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn init_debug_info(&mut self) {
        if self.safety_mode != SafetyMode::Debug {
            return;
        }
        let version = self.context.i32_type().const_int(3, false);
        self.module
            .add_basic_value_flag("Debug Info Version", FlagBehavior::Warning, version);
        let name = self.module.get_name().to_str().unwrap_or_default();
        let (builder, _) = self.module.create_debug_info_builder(
            true,
            DWARFSourceLanguage::C,
            name,
            ".",
            "zeru",
            false,
            "",
            0,
            "",
            DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );
        self.debug = Some(Debug {
            builder,
            files: HashMap::new(),
            scope: None,
        });
    }

    /// Open `function`'s scope at the line its declaration starts on.
    pub(super) fn enter_debug_scope(&mut self, function: FunctionValue<'ctx>, span: Span) {
        let (Some(debug), Some((path, line, _))) = (&mut self.debug, self.sources.position(span))
        else {
            return;
        };
        let Debug {
            builder,
            files,
            scope,
        } = debug;

        let file = *files.entry(path.to_string()).or_insert_with(|| {
            let path = Path::new(path);
            let name = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default();
            let directory = path.parent().and_then(|d| d.to_str()).unwrap_or_default();
            builder.create_file(name, directory)
        });
        let signature = builder.create_subroutine_type(file, None, &[], DIFlags::ZERO);
        let name = function.get_name().to_str().unwrap_or_default();
        let subprogram = builder.create_function(
            file.as_debug_info_scope(),
            name,
            None,
            file,
            line,
            signature,
            true,
            true,
            line,
            DIFlags::ZERO,
            false,
        );
        function.set_subprogram(subprogram);
        *scope = Some(subprogram);

        self.current_span = span;
        self.set_debug_location();
    }

    /// Tag what is emitted next with the statement being lowered.
    pub(super) fn set_debug_location(&self) {
        let (Some(debug), Some((_, line, column))) =
            (&self.debug, self.sources.position(self.current_span))
        else {
            return;
        };
        let Some(scope) = debug.scope else {
            return;
        };
        let location = debug.builder.create_debug_location(
            self.context,
            line,
            column,
            scope.as_debug_info_scope(),
            None,
        );
        self.builder.set_current_debug_location(location);
    }

    /// Put `scope` in place of the current one and return that, so a helper
    /// function can be emitted partway through another.
    pub(super) fn swap_debug_scope(
        &mut self,
        scope: Option<DISubprogram<'ctx>>,
    ) -> Option<DISubprogram<'ctx>> {
        let debug = self.debug.as_mut()?;
        std::mem::replace(&mut debug.scope, scope)
    }

    pub(super) fn leave_debug_scope(&mut self) {
        if let Some(debug) = &mut self.debug {
            debug.scope = None;
        }
        self.builder.unset_current_debug_location();
    }

    pub(super) fn finish_debug_info(&self) {
        if let Some(debug) = &self.debug {
            debug.builder.finalize();
        }
    }
}
