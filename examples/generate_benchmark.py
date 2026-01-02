#!/usr/bin/env python3
"""
Zeru Benchmark Code Generator

Generates syntactically valid Zeru code for benchmarking the compiler.
This produces large programs with random structs, methods, functions,
and operations to stress-test memory usage and compilation time.

Usage:
    python generate_benchmark.py --lines 1000000 --output stress_test.zr
    python generate_benchmark.py --lines 10000 --seed 42 --output deterministic.zr
"""

import argparse
import random
from dataclasses import dataclass
from typing import List, Optional

PRIMITIVE_TYPES = [
    "i8",
    "i16",
    "i32",
    "i64",
    "u8",
    "u16",
    "u32",
    "u64",
    "f32",
    "f64",
    "bool",
]
INTEGER_TYPES = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"]
FLOAT_TYPES = ["f32", "f64"]
BINARY_OPS = ["+", "-", "*", "/"]
COMPARISON_OPS = ["==", "!=", "<", ">", "<=", ">="]
LOGICAL_OPS = ["&&", "||"]
BITWISE_OPS = ["&", "|", "^", "<<", ">>"]


@dataclass
class StructDef:
    name: str
    fields: List[tuple]


@dataclass
class EnumDef:
    name: str
    variants: List[str]


@dataclass
class FunctionDef:
    name: str
    params: List[tuple]
    return_type: Optional[str]


class GeneratorState:
    def __init__(self, seed: Optional[int] = None):
        self.rng = random.Random(seed)
        self.structs: List[StructDef] = []
        self.enums: List[EnumDef] = []
        self.functions: List[FunctionDef] = []
        self.name_counter = 0
        self.line_count = 0

    def unique_name(self, prefix: str) -> str:
        self.name_counter += 1
        return f"{prefix}_{self.name_counter}"

    def random_primitive(self) -> str:
        return self.rng.choice(PRIMITIVE_TYPES)

    def random_integer_type(self) -> str:
        return self.rng.choice(INTEGER_TYPES)

    def random_float_type(self) -> str:
        return self.rng.choice(FLOAT_TYPES)

    def random_type(self, allow_complex: bool = True) -> str:
        """Return a random type, potentially including user-defined types."""
        choices = PRIMITIVE_TYPES.copy()

        if allow_complex and self.structs:
            choices.extend(s.name for s in self.structs)

        if allow_complex and self.rng.random() < 0.1:
            # Pointer type
            base = self.rng.choice(PRIMITIVE_TYPES)
            return f"*{base}"

        if allow_complex and self.rng.random() < 0.1:
            # Array type
            elem = self.rng.choice(PRIMITIVE_TYPES)
            size = self.rng.randint(1, 100)
            return f"Array<{elem}, {size}>"

        return self.rng.choice(choices)

    def random_value(self, ty: str) -> str:
        """Generate a random literal value for the given type."""
        if ty == "i8":
            return str(self.rng.randint(0, 127))
        elif ty == "i16":
            return str(self.rng.randint(0, 32767))
        elif ty == "i32":
            return str(self.rng.randint(0, 1000000))
        elif ty == "i64":
            return str(self.rng.randint(0, 1000000000))
        elif ty == "u8":
            return str(self.rng.randint(0, 255))
        elif ty == "u16":
            return str(self.rng.randint(0, 65535))
        elif ty in ["u32", "u64", "usize"]:
            return str(self.rng.randint(0, 1000000))
        elif ty in ["f32", "f64"]:
            return f"{self.rng.uniform(0.0, 100.0):.4f}"
        elif ty == "bool":
            return self.rng.choice(["true", "false"])
        elif ty.startswith("*"):
            return "0 as i64 as " + ty
        elif ty.startswith("Array<"):
            inner = ty[6:-1]
            parts = inner.rsplit(",", 1)
            elem_type = parts[0].strip()
            size = int(parts[1].strip())
            if size <= 10:
                vals = [self.random_value(elem_type) for _ in range(size)]
                return f"[{', '.join(vals)}]"
            else:
                return f"[{self.random_value(elem_type)}; {size}]"
        elif self.get_struct(ty):
            struct = self.get_struct(ty)
            fields = ", ".join(
                f"{f[0]}: {self.random_value(f[1])}" for f in struct.fields
            )
            return f"{ty} {{ {fields} }}"
        else:
            return "0"

    def get_struct(self, name: str) -> Optional[StructDef]:
        for s in self.structs:
            if s.name == name:
                return s
        return None


def generate_constant(state: GeneratorState) -> List[str]:
    """Generate a const declaration."""
    name = state.unique_name("CONST").upper()
    ty = state.random_primitive()
    value = state.random_value(ty)
    return [f"const {name}: {ty} = {value};"]


def generate_struct(state: GeneratorState, with_methods: bool = True) -> List[str]:
    """Generate a struct definition with optional methods."""
    lines = []
    name = state.unique_name("Struct")

    num_fields = state.rng.randint(1, 8)
    fields = []
    for i in range(num_fields):
        field_name = f"field_{i}"
        field_type = state.random_primitive()
        fields.append((field_name, field_type))

    struct_def = StructDef(name, fields)
    state.structs.append(struct_def)

    lines.append(f"struct {name} {{")
    for fname, ftype in fields:
        lines.append(f"    {fname}: {ftype},")

    if with_methods and state.rng.random() < 0.7:
        lines.append("")
        lines.append(
            f"    fn new({', '.join(f'{f[0]}: {f[1]}' for f in fields)}) {name} {{"
        )
        field_inits = ", ".join(f"{f[0]}: {f[0]}" for f in fields)
        lines.append(f"        return {name} {{ {field_inits} }};")
        lines.append("    }")

        for fname, ftype in fields:
            if state.rng.random() < 0.3:
                lines.append("")
                lines.append(f"    fn get_{fname}(self) {ftype} {{")
                lines.append(f"        return self.{fname};")
                lines.append("    }")

            if state.rng.random() < 0.3:
                lines.append("")
                lines.append(f"    fn set_{fname}(var self, value: {ftype}) {{")
                lines.append(f"        self.{fname} = value;")
                lines.append("    }")

        if state.rng.random() < 0.5 and fields:
            lines.append("")
            ret_type = state.random_primitive()
            lines.append(f"    fn compute(self) {ret_type} {{")
            numeric_fields = [
                (f, t) for f, t in fields if t in INTEGER_TYPES + FLOAT_TYPES
            ]
            if numeric_fields:
                f0 = numeric_fields[0]
                lines.append(
                    f"        var result: {ret_type} = self.{f0[0]} as {ret_type};"
                )
                for fname, ftype in numeric_fields[1:3]:  # Use up to 3 fields
                    op = state.rng.choice(["+", "-", "*"])
                    lines.append(
                        f"        result = result {op} (self.{fname} as {ret_type});"
                    )
                lines.append(f"        return result;")
            else:
                lines.append(f"        return {state.random_value(ret_type)};")
            lines.append("    }")

    lines.append("}")
    return lines


def generate_enum(state: GeneratorState) -> List[str]:
    """Generate an enum definition."""
    name = state.unique_name("Enum")
    num_variants = state.rng.randint(2, 8)
    variants = [f"Variant_{i}" for i in range(num_variants)]

    state.enums.append(EnumDef(name, variants))

    lines = [f"enum {name} {{"]
    for v in variants:
        lines.append(f"    {v},")
    lines.append("}")
    return lines


def generate_function(state: GeneratorState, complexity: int = 2) -> List[str]:
    """Generate a function with configurable complexity."""
    lines = []
    name = state.unique_name("func")

    num_params = state.rng.randint(0, 4)
    params = []
    for i in range(num_params):
        pname = f"param_{i}"
        ptype = state.random_primitive()
        is_mut = state.rng.random() < 0.2
        params.append((pname, ptype, is_mut))

    has_return = state.rng.random() < 0.7
    return_type = state.random_primitive() if has_return else None

    state.functions.append(FunctionDef(name, params, return_type))

    param_str = ", ".join(f"{'var ' if p[2] else ''}{p[0]}: {p[1]}" for p in params)
    ret_str = f" {return_type}" if return_type else ""
    lines.append(f"fn {name}({param_str}){ret_str} {{")

    local_vars = []

    num_locals = state.rng.randint(1, complexity * 2)
    for i in range(num_locals):
        vname = f"local_{i}"
        vtype = state.random_primitive()
        vvalue = state.random_value(vtype)
        local_vars.append((vname, vtype))
        lines.append(f"    var {vname}: {vtype} = {vvalue};")

    all_vars = [(p[0], p[1]) for p in params] + local_vars
    numeric_vars = [(n, t) for n, t in all_vars if t in INTEGER_TYPES + FLOAT_TYPES]

    for _ in range(complexity):
        if numeric_vars and state.rng.random() < 0.5:
            v1 = state.rng.choice(numeric_vars)
            v2 = (
                state.rng.choice(numeric_vars)
                if state.rng.random() < 0.7
                else (state.random_value(v1[1]), v1[1])
            )
            op = state.rng.choice(BINARY_OPS)
            target = state.rng.choice(
                [v for v in local_vars if v[1] in INTEGER_TYPES + FLOAT_TYPES]
                or local_vars
            )

            lines.append(
                f"    {target[0]} = ({v1[0]} as {target[1]}) {op} ({v2[0] if isinstance(v2[0], str) and not v2[0][0].isdigit() else v2[0]} as {target[1]});"
            )

        elif state.rng.random() < 0.3:
            int_vars = [(n, t) for n, t in numeric_vars if t in INTEGER_TYPES]
            if int_vars:
                cond_var = state.rng.choice(int_vars)
                lines.append(f"    if {cond_var[0]} > (0 as {cond_var[1]}) {{")
                if local_vars:
                    target = state.rng.choice(local_vars)
                    lines.append(
                        f"        {target[0]} = {state.random_value(target[1])};"
                    )
                lines.append("    }")

        elif state.rng.random() < 0.2:
            lines.append(f"    var loop_count_{state.name_counter}: i32 = 0;")
            lines.append(f"    while loop_count_{state.name_counter} < 5 {{")
            lines.append(f"        loop_count_{state.name_counter} += 1;")
            if numeric_vars:
                target = state.rng.choice(
                    [v for v in local_vars if v[1] in INTEGER_TYPES] or local_vars
                )
                lines.append(f"        {target[0]} += 1 as {target[1]};")
            lines.append("    }")

    # Retur
    if return_type:
        if return_type in INTEGER_TYPES + FLOAT_TYPES and numeric_vars:
            ret_var = state.rng.choice(numeric_vars)
            lines.append(f"    return {ret_var[0]} as {return_type};")
        else:
            lines.append(f"    return {state.random_value(return_type)};")

    lines.append("}")
    return lines


def generate_main_function(
    state: GeneratorState, call_functions: bool = True
) -> List[str]:
    """Generate a main function that exercises the generated code."""
    lines = ["fn main() {"]

    for struct in state.structs[:5]:
        vname = state.unique_name("s")
        fields = ", ".join(f"{f[0]}: {state.random_value(f[1])}" for f in struct.fields)
        lines.append(f"    var {vname}: {struct.name} = {struct.name} {{ {fields} }};")

    if call_functions:
        for func in state.functions[:10]:
            args = ", ".join(state.random_value(p[1]) for p in func.params)
            if func.return_type:
                vname = state.unique_name("result")
                lines.append(
                    f"    var {vname}: {func.return_type} = {func.name}({args});"
                )
            else:
                lines.append(f"    {func.name}({args});")

    lines.append("}")
    return lines


def generate_benchmark_code(
    target_lines: int, seed: Optional[int] = None, complexity: int = 3
) -> str:
    """Generate a complete Zeru benchmark program."""
    state = GeneratorState(seed)
    all_lines: List[str] = []

    all_lines.extend(
        [
            "// Auto-generated Zeru benchmark code",
            f"// Target lines: {target_lines}",
            f"// Seed: {seed}",
            "",
        ]
    )

    estimated_lines_per_const = 1
    estimated_lines_per_struct = 20
    estimated_lines_per_enum = 6
    estimated_lines_per_function = 15

    remaining = target_lines - 20

    num_consts = max(5, remaining // 100)
    remaining -= num_consts * estimated_lines_per_const

    num_structs = max(5, remaining // 4 // estimated_lines_per_struct)
    remaining -= num_structs * estimated_lines_per_struct

    num_enums = max(3, remaining // 10 // estimated_lines_per_enum)
    remaining -= num_enums * estimated_lines_per_enum

    num_functions = max(10, remaining // estimated_lines_per_function)

    print(
        f"Generating: {num_consts} consts, {num_structs} structs, "
        f"{num_enums} enums, {num_functions} functions"
    )

    all_lines.append("// ============ Constants ============")
    for _ in range(num_consts):
        all_lines.extend(generate_constant(state))
    all_lines.append("")

    all_lines.append("// ============ Enums ============")
    for _ in range(num_enums):
        all_lines.extend(generate_enum(state))
        all_lines.append("")

    all_lines.append("// ============ Structs ============")
    for _ in range(num_structs):
        all_lines.extend(generate_struct(state, with_methods=True))
        all_lines.append("")

    all_lines.append("// ============ Functions ============")
    for _ in range(num_functions):
        func_complexity = state.rng.randint(1, complexity)
        all_lines.extend(generate_function(state, complexity=func_complexity))
        all_lines.append("")

    while len(all_lines) < target_lines - 30:
        all_lines.extend(generate_function(state, complexity=complexity))
        all_lines.append("")

    all_lines.append("// ============ Main ============")
    all_lines.extend(generate_main_function(state, call_functions=True))

    return "\n".join(all_lines)


def main():
    parser = argparse.ArgumentParser(
        description="Generate Zeru benchmark code for compiler stress testing"
    )
    parser.add_argument(
        "--lines",
        "-l",
        type=int,
        default=10000,
        help="Target number of lines to generate (default: 10000)",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=str,
        default="benchmark_generated.zr",
        help="Output file path (default: benchmark_generated.zr)",
    )
    parser.add_argument(
        "--seed",
        "-s",
        type=int,
        default=None,
        help="Random seed for reproducible generation",
    )
    parser.add_argument(
        "--complexity",
        "-c",
        type=int,
        default=3,
        choices=[1, 2, 3, 4, 5],
        help="Function complexity level 1-5 (default: 3)",
    )
    parser.add_argument(
        "--stdout", action="store_true", help="Print to stdout instead of file"
    )

    args = parser.parse_args()

    print(f"Generating {args.lines} lines of Zeru code...")
    code = generate_benchmark_code(
        target_lines=args.lines, seed=args.seed, complexity=args.complexity
    )

    actual_lines = code.count("\n") + 1
    print(f"Generated {actual_lines} lines")

    if args.stdout:
        print(code)
    else:
        with open(args.output, "w") as f:
            f.write(code)
        print(f"Written to: {args.output}")


if __name__ == "__main__":
    main()
