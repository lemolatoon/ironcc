.intel_syntax noprefix

.global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 0
  push 3
  sub rsp, 8
  call test0
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 0
  push 2
  sub rsp, 8
  call test0_2
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 0
  push 10
  sub rsp, 8
  call test0_3
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 1
  push 9
  sub rsp, 8
  call test1
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 2
  push 5
  push 5
  pop rdi
  sub rsp, 8
  call test2
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 3
  push 5
  push 5
  pop rdi
  sub rsp, 8
  call test3
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 4
  push 2
  push 3
  pop rdi
  sub rsp, 8
  call test4
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 4
  push 3
  push 4
  pop rdi
  sub rsp, 8
  call test4
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 5
  push 10
  push 5
  pop rdi
  sub rsp, 8
  call test5
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 6
  push 2
  push 2
  pop rdi
  sub rsp, 8
  call test6
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 7
  push 0
  push 1
  pop rdi
  sub rsp, 8
  call test7
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 7
  push 0
  push 91
  pop rdi
  sub rsp, 8
  call test7
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 7
  push 1
  push 109
  pop rdi
  sub rsp, 8
  call test7
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 8
  push 3
  sub rsp, 8
  call test8
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  sub rsp, 8
  call test9_0
  add rsp, 8
  push rax
  pop rax
  sub rsp, 8
  call test9_1
  add rsp, 8
  push rax
  pop rax
  push 9
  push 10
  sub rsp, 8
  call test9
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 10
  push 3
  sub rsp, 8
  call test10
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 11
  push 0
  sub rsp, 8
  call test11
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  push 12
  push 0
  sub rsp, 8
  call test12
  add rsp, 8
  push rax
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call assert
  add rsp, 8
  push rax
  pop rax
  sub rsp, 8
  call print_ok
  add rsp, 8
  push rax
  pop rax
  push 0
  pop rax
  jmp .Lmain_ret
.Lmain_ret:
  mov rsp, rbp
  pop rbp
  ret
test0:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 3
  pop rax
  jmp .Ltest0_ret
.Ltest0_ret:
  mov rsp, rbp
  pop rbp
  ret
test0_2:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 2
  pop rdi
  sub rsp, 8
  call g
  add rsp, 8
  push rax
  pop rax
  jmp .Ltest0_2_ret
.Ltest0_2_ret:
  mov rsp, rbp
  pop rbp
  ret
test0_3:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 5
  pop rdi
  sub rsp, 8
  call h
  add rsp, 8
  push rax
  pop rax
  jmp .Ltest0_3_ret
.Ltest0_3_ret:
  mov rsp, rbp
  pop rbp
  ret
g:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Lg_ret
.Lg_ret:
  mov rsp, rbp
  pop rbp
  ret
h:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  push 5
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  cmp rax, rdi
  setl al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lelse1
  push 5
  pop rax
  jmp .Lh_ret
  jmp .Lend1
.Lelse1:
  push 10
  pop rax
  jmp .Lh_ret
.Lend1:
.Lh_ret:
  mov rsp, rbp
  pop rbp
  ret
test1:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  push 5
  pop rdi
  sub rsp, 8
  call f
  add rsp, 8
  push rax
  pop rax
  jmp .Ltest1_ret
.Ltest1_ret:
  mov rsp, rbp
  pop rbp
  ret
f:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 8
  push rax
  push 3
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  push 4
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  cmp rax, rdi
  setl al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend2
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  sub rsp, 8
  call f
  add rsp, 8
  push rax
  push 2
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Lf_ret
.Lend2:
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Lf_ret
.Lf_ret:
  mov rsp, rbp
  pop rbp
  ret
test2:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 0
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend3
  push 0
  pop rax
  jmp .Ltest2_ret
.Lend3:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend4
  push 1
  pop rax
  jmp .Ltest2_ret
.Lend4:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Ltest2_ret
.Ltest2_ret:
  mov rsp, rbp
  pop rbp
  ret
test3:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 0
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend5
  push 0
  pop rax
  jmp .Ltest3_ret
.Lend5:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend6
  push 1
  pop rax
  jmp .Ltest3_ret
.Lend6:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  sub rsp, 8
  call test3
  add rsp, 8
  push rax
  push 1
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Ltest3_ret
.Ltest3_ret:
  mov rsp, rbp
  pop rbp
  ret
test4:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 0
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend7
  push 0
  pop rax
  jmp .Ltest4_ret
.Lend7:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend8
  push 1
  pop rax
  jmp .Ltest4_ret
.Lend8:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  sub rsp, 8
  call test4
  add rsp, 8
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 2
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  call test4
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Ltest4_ret
.Ltest4_ret:
  mov rsp, rbp
  pop rbp
  ret
test5:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1000000
  pop rsi
  pop rdi
  sub rsp, 8
  call add
  add rsp, 8
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1000000
  pop rsi
  pop rdi
  call sub
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Ltest5_ret
.Ltest5_ret:
  mov rsp, rbp
  pop rbp
  ret
test6:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 0
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend9
  push 1
  pop rax
  jmp .Ltest6_ret
.Lend9:
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  call test6
  push rax
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rax
  jmp .Ltest6_ret
.Ltest6_ret:
  mov rsp, rbp
  pop rbp
  ret
test7:
  push rbp
  mov rbp, rsp
  sub rsp, 24
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend10
  push 0
  pop rax
  jmp .Ltest7_ret
.Lend10:
  mov rax, rbp
  sub rax, 8
  push rax
  push 1
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  push 2
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
.Lbegin11:
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  cmp rax, rdi
  setl al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend11
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  cqo
  idiv rdi
  mov rax, rdx
  push rax
  push 0
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend12
  mov rax, rbp
  sub rax, 8
  push rax
  push 0
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
.Lend12:
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 1
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  jmp .Lbegin11
.Lend11:
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Ltest7_ret
.Ltest7_ret:
  mov rsp, rbp
  pop rbp
  ret
test8:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rbp
  sub rax, 4
  push rax
  push 3
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Ltest8_ret
.Ltest8_ret:
  mov rsp, rbp
  pop rbp
  ret
test9_0:
  push rbp
  mov rbp, rsp
  sub rsp, 40
  mov rax, rbp
  sub rax, 4
  push rax
  push 10
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 16
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 90
  push 10
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
.Ltest9_0_ret:
  mov rsp, rbp
  pop rbp
  ret
test9_1:
  push rbp
  mov rbp, rsp
  sub rsp, 40
  mov rax, rbp
  sub rax, 4
  push rax
  push 10
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 16
  push rax
  push 5
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 16
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 91
  push 5
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 91
  push 10
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
.Ltest9_1_ret:
  mov rsp, rbp
  pop rbp
  ret
test9:
  push rbp
  mov rbp, rsp
  sub rsp, 40
  mov rax, rbp
  sub rax, 4
  push rax
  push 5
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  push 9
  push 5
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 8
  push rax
  push 10
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  push 9
  push 10
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 16
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 9
  push 5
  mov rax, rbp
  sub rax, 16
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  push 9
  push 10
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  mov rax, rbp
  sub rax, 16
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 9
  push 5
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  push 5
  pop rdi
  pop rax
  cmp rax, rdi
  sete al
  movzx rax, al
  push rax
  pop rax
  cmp rax, 0
  je .Lend13
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 9
  push 10
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
.Lend13:
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Ltest9_ret
.Ltest9_ret:
  mov rsp, rbp
  pop rbp
  ret
test10:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rax
  jmp .Ltest10_ret
.Ltest10_ret:
  mov rsp, rbp
  pop rbp
  ret
test11:
  push rbp
  mov rbp, rsp
  sub rsp, 24
  mov rax, rbp
  sub rax, 8
  push rax
  push 2
  push 3
  push 5
  push 8
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call alloc4
  add rsp, 8
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 16
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 2
  push 4
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 11
  push 5
  mov rax, rbp
  sub rax, 16
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 16
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 4
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 11
  push 8
  mov rax, rbp
  sub rax, 16
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  push 1
  push 2
  push 3
  push 4
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call alloc4
  add rsp, 8
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 11
  push 4
  push 4
  push 3
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  push 0
  pop rax
  jmp .Ltest11_ret
.Ltest11_ret:
  mov rsp, rbp
  pop rbp
  ret
test12:
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov rax, rbp
  sub rax, 4
  push rax
  push 1
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 8
  push rax
  push 2
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 12
  push rax
  push 3
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 16
  push rax
  push 4
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 4
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  mov rax, rbp
  sub rax, 12
  push rax
  mov rax, rbp
  sub rax, 16
  push rax
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  sub rsp, 8
  call alloc4_ptr
  add rsp, 8
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 12
  push 1
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 2
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 12
  push 3
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 2
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 32
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  push 12
  push 4
  mov rax, rbp
  sub rax, 32
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  push 12
  push 4
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 9
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  push 12
  push 9
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  mov rax, rbp
  sub rax, 44
  push rax
  push 1
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 40
  push rax
  mov rax, rbp
  sub rax, 44
  push rax
  pop rdi
  pop rax
  mov QWORD PTR [rax], rdi
  push rdi
  pop rax
  mov rax, rbp
  sub rax, 40
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 1
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 2
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  mov rax, rbp
  sub rax, 24
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  push 3
  push 8
  pop rdi
  pop rax
  imul rax, rdi
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rdi
  pop rax
  mov DWORD PTR [rax], edi
  push rdi
  pop rax
  push 12
  push 0
  push 3
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  mov rax, rbp
  sub rax, 40
  push rax
  pop rax
  mov rax, QWORD PTR [rax]
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdx
  pop rsi
  pop rdi
  call assert
  push rax
  pop rax
  push 0
  pop rax
  jmp .Ltest12_ret
.Ltest12_ret:
  mov rsp, rbp
  pop rbp
  ret
add:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 8
  mov DWORD PTR [rax], esi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  add rax, rdi
  push rax
  pop rax
  jmp .Ladd_ret
.Ladd_ret:
  mov rsp, rbp
  pop rbp
  ret
sub:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rbp
  sub rax, 4
  mov DWORD PTR [rax], edi
  mov rax, rbp
  sub rax, 8
  mov DWORD PTR [rax], esi
  mov rax, rbp
  sub rax, 4
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  mov rax, rbp
  sub rax, 8
  push rax
  pop rax
  mov eax, DWORD PTR [rax]
  push rax
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rax
  jmp .Lsub_ret
.Lsub_ret:
  mov rsp, rbp
  pop rbp
  ret
