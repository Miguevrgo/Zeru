# What a program needs that libc used to give it, for x86-64 Linux: the
# process entry, and the functions LLVM calls by name for copies, string
# lengths, float remainders and truncation, and for a panic.

    .text

    .globl _start
    .type _start, @function
_start:
    xor %ebp, %ebp
    mov %rsp, __zeru_stack(%rip)
    and $-16, %rsp
    call __zeru_detect_simd
    call __zeru_start
    mov %eax, %edi
    mov $60, %eax
    syscall

# The widest vectors the processor and the kernel allow, as glibc chooses
# them: 2 for AVX-512, 1 for AVX2, 0 for the SSE2 every x86-64 has.
__zeru_detect_simd:
    push %rbx
    mov $1, %eax
    cpuid
    and $0x18000000, %ecx
    cmp $0x18000000, %ecx
    jne 2f
    xor %ecx, %ecx
    xgetbv
    mov %eax, %r8d
    mov $7, %eax
    xor %ecx, %ecx
    cpuid
    mov %r8d, %eax
    and $0xe6, %eax
    cmp $0xe6, %eax
    jne 1f
    bt $16, %ebx
    jnc 1f
    movb $2, __zeru_simd(%rip)
    jmp 2f
1:
    and $6, %r8d
    cmp $6, %r8d
    jne 2f
    bt $5, %ebx
    jnc 2f
    movb $1, __zeru_simd(%rip)
2:
    pop %rbx
    ret

    .bss
__zeru_simd:
    .byte 0

# The stack as the kernel handed it to _start: argc, then argv, then envp.
    .globl __zeru_stack
    .p2align 3
__zeru_stack:
    .quad 0

# The environment once std.os changes it: a Vec<String> of "NAME=value",
# then whether it holds the kernel's environment yet.
    .globl __zeru_env
    .p2align 3
__zeru_env:
    .quad 0, 0, 0, 0

# The one fs::File std.os reads standard input through, so what it buffers
# stays for the next read: all zero is fd 0 with an empty buffer.
    .globl __zeru_stdin
    .p2align 3
__zeru_stdin:
    .quad 0, 0, 0, 0, 0
    .text

# Copies of up to 64 bytes read everything into registers before writing,
# so they are right for overlapping buffers too. Larger ones read the first
# and last 64 bytes first, move the 64-byte blocks of the destination's
# aligned middle away from the overlap, each read whole before it is
# written, and write the two ends last; from 2 KiB a forward copy is rep
# movsb, which the processor runs fastest.
    .globl memcpy
    .type memcpy, @function
    .globl memmove
    .type memmove, @function
memcpy:
memmove:
    mov %rdi, %rax
    cmp $16, %rdx
    ja 3f
    cmp $8, %rdx
    jb 1f
    mov (%rsi), %r8
    mov -8(%rsi,%rdx), %r9
    mov %r8, (%rdi)
    mov %r9, -8(%rdi,%rdx)
    ret
1:
    cmp $4, %rdx
    jb 2f
    mov (%rsi), %r8d
    mov -4(%rsi,%rdx), %r9d
    mov %r8d, (%rdi)
    mov %r9d, -4(%rdi,%rdx)
    ret
2:
    test %rdx, %rdx
    jz 9f
    movzbl (%rsi), %r8d
    movzbl -1(%rsi,%rdx), %r9d
    cmp $2, %rdx
    jb 8f
    movzwl (%rsi), %r8d
    movzwl -2(%rsi,%rdx), %r9d
    mov %r8w, (%rdi)
    mov %r9w, -2(%rdi,%rdx)
    ret
8:
    mov %r8b, (%rdi)
9:
    ret
3:
    cmp $32, %rdx
    ja 4f
    movdqu (%rsi), %xmm0
    movdqu -16(%rsi,%rdx), %xmm1
    movdqu %xmm0, (%rdi)
    movdqu %xmm1, -16(%rdi,%rdx)
    ret
4:
    cmp $64, %rdx
    ja 5f
    movdqu (%rsi), %xmm0
    movdqu 16(%rsi), %xmm1
    movdqu -32(%rsi,%rdx), %xmm2
    movdqu -16(%rsi,%rdx), %xmm3
    movdqu %xmm0, (%rdi)
    movdqu %xmm1, 16(%rdi)
    movdqu %xmm2, -32(%rdi,%rdx)
    movdqu %xmm3, -16(%rdi,%rdx)
    ret
5:
    mov %rdi, %rcx
    sub %rsi, %rcx
    cmp %rdx, %rcx
    jb .Lbackward
    cmp $2048, %rdx
    jb .Lforward
    mov %rdx, %rcx
    rep movsb
    ret

.Lforward:
    lea 64(%rdi), %rcx
    and $-64, %rcx
    sub %rdi, %rcx
    lea -64(%rdx), %r8
    cmpb $1, __zeru_simd(%rip)
    je .Lforward_avx2
    ja .Lforward_avx512
    movdqu (%rsi), %xmm4
    movdqu 16(%rsi), %xmm5
    movdqu 32(%rsi), %xmm6
    movdqu 48(%rsi), %xmm7
    movdqu (%rsi,%r8), %xmm8
    movdqu 16(%rsi,%r8), %xmm9
    movdqu 32(%rsi,%r8), %xmm10
    movdqu 48(%rsi,%r8), %xmm11
1:
    cmp %r8, %rcx
    jae 2f
    movdqu (%rsi,%rcx), %xmm0
    movdqu 16(%rsi,%rcx), %xmm1
    movdqu 32(%rsi,%rcx), %xmm2
    movdqu 48(%rsi,%rcx), %xmm3
    movdqa %xmm0, (%rdi,%rcx)
    movdqa %xmm1, 16(%rdi,%rcx)
    movdqa %xmm2, 32(%rdi,%rcx)
    movdqa %xmm3, 48(%rdi,%rcx)
    add $64, %rcx
    jmp 1b
2:
    movdqu %xmm8, (%rdi,%r8)
    movdqu %xmm9, 16(%rdi,%r8)
    movdqu %xmm10, 32(%rdi,%r8)
    movdqu %xmm11, 48(%rdi,%r8)
    movdqu %xmm4, (%rdi)
    movdqu %xmm5, 16(%rdi)
    movdqu %xmm6, 32(%rdi)
    movdqu %xmm7, 48(%rdi)
    ret

.Lforward_avx2:
    vmovdqu (%rsi), %ymm2
    vmovdqu 32(%rsi), %ymm3
    vmovdqu (%rsi,%r8), %ymm4
    vmovdqu 32(%rsi,%r8), %ymm5
1:
    cmp %r8, %rcx
    jae 2f
    vmovdqu (%rsi,%rcx), %ymm0
    vmovdqu 32(%rsi,%rcx), %ymm1
    vmovdqa %ymm0, (%rdi,%rcx)
    vmovdqa %ymm1, 32(%rdi,%rcx)
    add $64, %rcx
    jmp 1b
2:
    vmovdqu %ymm4, (%rdi,%r8)
    vmovdqu %ymm5, 32(%rdi,%r8)
    vmovdqu %ymm2, (%rdi)
    vmovdqu %ymm3, 32(%rdi)
    vzeroupper
    ret

.Lforward_avx512:
    vmovdqu64 (%rsi), %zmm1
    vmovdqu64 (%rsi,%r8), %zmm2
1:
    cmp %r8, %rcx
    jae 2f
    vmovdqu64 (%rsi,%rcx), %zmm0
    vmovdqa64 %zmm0, (%rdi,%rcx)
    add $64, %rcx
    jmp 1b
2:
    vmovdqu64 %zmm2, (%rdi,%r8)
    vmovdqu64 %zmm1, (%rdi)
    vzeroupper
    ret

.Lbackward:
    lea (%rdi,%rdx), %rcx
    and $-64, %rcx
    sub %rdi, %rcx
    sub $64, %rcx
    lea -64(%rdx), %r8
    cmpb $1, __zeru_simd(%rip)
    je .Lbackward_avx2
    ja .Lbackward_avx512
    movdqu (%rsi), %xmm4
    movdqu 16(%rsi), %xmm5
    movdqu 32(%rsi), %xmm6
    movdqu 48(%rsi), %xmm7
    movdqu (%rsi,%r8), %xmm8
    movdqu 16(%rsi,%r8), %xmm9
    movdqu 32(%rsi,%r8), %xmm10
    movdqu 48(%rsi,%r8), %xmm11
1:
    cmp $0, %rcx
    jle 2f
    movdqu (%rsi,%rcx), %xmm0
    movdqu 16(%rsi,%rcx), %xmm1
    movdqu 32(%rsi,%rcx), %xmm2
    movdqu 48(%rsi,%rcx), %xmm3
    movdqa %xmm0, (%rdi,%rcx)
    movdqa %xmm1, 16(%rdi,%rcx)
    movdqa %xmm2, 32(%rdi,%rcx)
    movdqa %xmm3, 48(%rdi,%rcx)
    sub $64, %rcx
    jmp 1b
2:
    movdqu %xmm8, (%rdi,%r8)
    movdqu %xmm9, 16(%rdi,%r8)
    movdqu %xmm10, 32(%rdi,%r8)
    movdqu %xmm11, 48(%rdi,%r8)
    movdqu %xmm4, (%rdi)
    movdqu %xmm5, 16(%rdi)
    movdqu %xmm6, 32(%rdi)
    movdqu %xmm7, 48(%rdi)
    ret

.Lbackward_avx2:
    vmovdqu (%rsi), %ymm2
    vmovdqu 32(%rsi), %ymm3
    vmovdqu (%rsi,%r8), %ymm4
    vmovdqu 32(%rsi,%r8), %ymm5
1:
    cmp $0, %rcx
    jle 2f
    vmovdqu (%rsi,%rcx), %ymm0
    vmovdqu 32(%rsi,%rcx), %ymm1
    vmovdqa %ymm0, (%rdi,%rcx)
    vmovdqa %ymm1, 32(%rdi,%rcx)
    sub $64, %rcx
    jmp 1b
2:
    vmovdqu %ymm4, (%rdi,%r8)
    vmovdqu %ymm5, 32(%rdi,%r8)
    vmovdqu %ymm2, (%rdi)
    vmovdqu %ymm3, 32(%rdi)
    vzeroupper
    ret

.Lbackward_avx512:
    vmovdqu64 (%rsi), %zmm1
    vmovdqu64 (%rsi,%r8), %zmm2
1:
    cmp $0, %rcx
    jle 2f
    vmovdqu64 (%rsi,%rcx), %zmm0
    vmovdqa64 %zmm0, (%rdi,%rcx)
    sub $64, %rcx
    jmp 1b
2:
    vmovdqu64 %zmm2, (%rdi,%r8)
    vmovdqu64 %zmm1, (%rdi)
    vzeroupper
    ret

# The byte spread over a register, written as memcpy writes: overlapping
# stores at both ends up to 64 bytes, 64-byte blocks beyond, rep stosb from
# 2 KiB.
    .globl memset
    .type memset, @function
memset:
    mov %rdi, %rax
    movzbl %sil, %esi
    movabs $0x0101010101010101, %r8
    imul %r8, %rsi
    cmp $16, %rdx
    ja 3f
    cmp $8, %rdx
    jb 1f
    mov %rsi, (%rdi)
    mov %rsi, -8(%rdi,%rdx)
    ret
1:
    cmp $4, %rdx
    jb 2f
    mov %esi, (%rdi)
    mov %esi, -4(%rdi,%rdx)
    ret
2:
    test %rdx, %rdx
    jz 9f
    mov %sil, (%rdi)
    mov %sil, -1(%rdi,%rdx)
    cmp $2, %rdx
    jbe 9f
    mov %si, (%rdi)
9:
    ret
3:
    movq %rsi, %xmm0
    punpcklqdq %xmm0, %xmm0
    cmp $32, %rdx
    ja 4f
    movdqu %xmm0, (%rdi)
    movdqu %xmm0, -16(%rdi,%rdx)
    ret
4:
    cmp $64, %rdx
    ja 5f
    movdqu %xmm0, (%rdi)
    movdqu %xmm0, 16(%rdi)
    movdqu %xmm0, -32(%rdi,%rdx)
    movdqu %xmm0, -16(%rdi,%rdx)
    ret
5:
    cmp $2048, %rdx
    jb 6f
    mov %rdi, %r9
    mov %esi, %eax
    mov %rdx, %rcx
    rep stosb
    mov %r9, %rax
    ret
6:
    lea -64(%rdx), %r8
    xor %ecx, %ecx
    cmpb $1, __zeru_simd(%rip)
    je .Lset_avx2
    ja .Lset_avx512
1:
    movdqu %xmm0, (%rdi,%rcx)
    movdqu %xmm0, 16(%rdi,%rcx)
    movdqu %xmm0, 32(%rdi,%rcx)
    movdqu %xmm0, 48(%rdi,%rcx)
    add $64, %rcx
    cmp %r8, %rcx
    jb 1b
    movdqu %xmm0, (%rdi,%r8)
    movdqu %xmm0, 16(%rdi,%r8)
    movdqu %xmm0, 32(%rdi,%r8)
    movdqu %xmm0, 48(%rdi,%r8)
    ret

.Lset_avx2:
    vpbroadcastq %xmm0, %ymm0
1:
    vmovdqu %ymm0, (%rdi,%rcx)
    vmovdqu %ymm0, 32(%rdi,%rcx)
    add $64, %rcx
    cmp %r8, %rcx
    jb 1b
    vmovdqu %ymm0, (%rdi,%r8)
    vmovdqu %ymm0, 32(%rdi,%r8)
    vzeroupper
    ret

.Lset_avx512:
    vpbroadcastq %rsi, %zmm0
1:
    vmovdqu64 %zmm0, (%rdi,%rcx)
    add $64, %rcx
    cmp %r8, %rcx
    jb 1b
    vmovdqu64 %zmm0, (%rdi,%r8)
    vzeroupper
    ret

    .globl memcmp
    .type memcmp, @function
    .globl bcmp
    .type bcmp, @function
memcmp:
bcmp:
    xor %eax, %eax
    test %rdx, %rdx
    jz 2f
1:
    movzbl (%rdi), %eax
    movzbl (%rsi), %ecx
    sub %ecx, %eax
    jnz 2f
    inc %rdi
    inc %rsi
    dec %rdx
    jnz 1b
2:
    ret

# The bytes before the first zero, for a loop LLVM recognises as strlen.
    .globl strlen
    .type strlen, @function
strlen:
    mov %rdi, %rax
1:
    cmpb $0, (%rax)
    je 2f
    inc %rax
    jmp 1b
2:
    sub %rdi, %rax
    ret

# Clear the fraction bits below the binary point; what is already whole,
# infinite or NaN stays as it is, and what is under 1 keeps only its sign.
    .globl trunc
    .type trunc, @function
trunc:
    movq %xmm0, %rax
    mov %rax, %rcx
    shr $52, %rcx
    and $0x7ff, %ecx
    cmp $1075, %ecx
    jae 2f
    cmp $1023, %ecx
    jb 1f
    mov $1075, %edx
    sub %ecx, %edx
    mov %edx, %ecx
    mov $-1, %rdx
    shl %cl, %rdx
    and %rdx, %rax
    movq %rax, %xmm0
    ret
1:
    movabs $0x8000000000000000, %rdx
    and %rdx, %rax
    movq %rax, %xmm0
2:
    ret

    .globl truncf
    .type truncf, @function
truncf:
    movd %xmm0, %eax
    mov %eax, %ecx
    shr $23, %ecx
    and $0xff, %ecx
    cmp $150, %ecx
    jae 2f
    cmp $127, %ecx
    jb 1f
    mov $150, %edx
    sub %ecx, %edx
    mov %edx, %ecx
    mov $-1, %edx
    shl %cl, %edx
    and %edx, %eax
    movd %eax, %xmm0
    ret
1:
    and $0x80000000, %eax
    movd %eax, %xmm0
2:
    ret

# The x87 partial remainder, repeated until complete, is exact, as in musl.
    .globl fmod
    .type fmod, @function
fmod:
    movsd %xmm1, -16(%rsp)
    movsd %xmm0, -8(%rsp)
    fldl -16(%rsp)
    fldl -8(%rsp)
1:
    fprem
    fnstsw %ax
    testb $4, %ah
    jnz 1b
    fstp %st(1)
    fstpl -8(%rsp)
    movsd -8(%rsp), %xmm0
    ret

    .globl fmodf
    .type fmodf, @function
fmodf:
    movss %xmm1, -8(%rsp)
    movss %xmm0, -4(%rsp)
    flds -8(%rsp)
    flds -4(%rsp)
1:
    fprem
    fnstsw %ax
    testb $4, %ah
    jnz 1b
    fstp %st(1)
    fstps -4(%rsp)
    movss -4(%rsp), %xmm0
    ret

# Raise SIGABRT, as libc's abort does; exit with its status if that
# returns.
    .globl abort
    .type abort, @function
abort:
    mov $39, %eax
    syscall
    mov %eax, %edi
    mov $6, %esi
    mov $62, %eax
    syscall
    mov $134, %edi
    mov $60, %eax
    syscall
