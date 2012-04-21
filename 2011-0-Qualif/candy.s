	# I'm not doing any retries or buffering, so it'll have to
	# all come in one call or fail.  The problem statement mostly
	# constrains the input data size to under 1M, so it should be
	# ok with a redirect on most systems.  But interactive mode
	# definitely won't work :->
	
	# Read input in

	mov	$input, %esi
	mov	$1048576, %edx
	xor	%edi, %edi
	xor	%eax,%eax
	syscall
	mov	$input, %r15

	# Read test count
	call	next_int
	
	# Initialize accumulators
test:	push	%ax
	mov	$1000000, %r12
	mov	$0, %r13
	mov	$0, %r14

	# Read candy count
	call	next_int
	mov	%rax, %r11

	# Read next candy value
candy:	test	%r11,%r11
	jz	print
	call	next_int

	# Update accumulators
	cmp	%r12, %rax
	jge	wdone
	mov	%rax, %r12	# keep worst's value
wdone:	add	%rax, %r13	# keep sum
	xor	%rax, %r14	# keep "almost" sum

	# More candy?
	dec	%r11
	jmp	candy

	# Display results header
print:	movl	$data, %esi	# "Case #"
	movl	$6, %edx
	movl	$1, %edi
	mov	$1,%eax
	syscall
	incw	tt(%rip)
	movswl	tt(%rip), %eax	# test counter
	call	convert
	movl	$data+6, %esi	# ": "
	movl	$2, %edx
	movl	$1, %edi
	mov	$1,%eax
	syscall

	# Check result category
	cmp	$0, %r14
	je	yes
	movl	$data+8, %esi	# "NO"
	movl	$2, %edx
	movl	$1, %edi
	mov	$1,%eax
	syscall
	jmp	eol
yes:	mov	%r13, %rax	# sum - worst
	sub	%r12, %rax
	call	convert
eol:	movl	$data+10, %esi	# \n
	movl	$1, %edx
	movl	$1, %edi
	mov	$1,%eax
	syscall

	# Check test counter and loop
	pop	%ax
	cmp	tt(%rip), %ax
	jne	test

	# And quit
	mov	$60,%rax
	syscall

next_int:
	# Skip non-decimal
	mov	%r15, %rdx
skip:	mov	%rdx, %rax
	inc	%rdx
	mov	(%rax), %cl
	sub	$'0', %ecx
	cmp	$9, %cl
	ja	skip
	mov	$0, %rdx
	jmp	check
	# Then convert
encode: imul	$10, %rdx, %rdx
	movsbq	%cl, %rcx
	lea	-'0'(%rdx,%rcx), %rdx
check:	mov	(%rax), %cl
	mov	%rax, %r15
	inc	%rax
	lea	-'0'(%rcx), %edi
	cmp	$9, %dil
	jbe	encode
	mov	%rdx, %rax
	ret

convert:
	mov	$buf+10, %esi
	mov	$10, %ecx
	# Check for termination
cloop:	test	%eax, %eax
	je	pr
	# Divide
	cltd
	div	%ecx
	add	$'0', %edx
	dec	%rsi
	mov	%dl, (%rsi)
	jmp	cloop
	# Print
pr:	mov	$buf+10, %edx
	mov	$1, %edi
	sub	%rsi, %rdx
	mov	$1,%eax
	syscall
	ret

	.data
data:	.ascii "Case #: NO\n"

	.bss
tt:	.word 	0
buf:	.space	10
input:	.space	1048576
