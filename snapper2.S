.text

        .global _start

_start:

        /*
           Read the whole file, once.
           Buffer is sized for large input set.
           It must come all at once, though, Or Else...
        */
        movl    $buf_s,%edx
        movl    $buf,%ecx
        movl    $0,%ebx         # stdin
        movl    $3,%eax         # read
        int     $0x80

        /*
           Read number of tests T to %ebp
           Save current test index to stack
        */
        movl    $buf,%esi
        call    atoi
        movl    %eax,%ebp       # number of tests
        movl    $0,%eax         # current test
        pushl   %eax

test:
        /*
           Check current test number against T
           Quit if done
        */
        popl    %eax
        incl    %eax            # increment test counter
        cmpl    %ebp,%eax
        jbe     noquit
        movl    $0,%ebx         # done -> quit
        movl    $1,%eax
        int     $0x80
noquit: pushl   %eax

        /* Print out test introduction string */
        movl    $case_s,%edx
        movl    $case,%ecx
        movl    $1,%ebx
        movl    $4,%eax
        int     $0x80           # "Case #"

        popl    %eax
        pushl   %eax

        /* Convert current test number to ascii */
        movl    $num,%edi
        addl    $num_s,%edi
        movl    %edi,%ebx       # save end of buffer to remember length
        movl    $10,%ecx
itoa_c:
        movl    $0,%edx
        divl    %ecx            # divide by 10
        addb    $'0',%dl
        decl    %edi
        movb    %dl,(%edi)      # store remainder in buffer
        cmpl    $0,%eax
        jnz     itoa_c          # loop not zero yet

        /* Print current test number */
	movl    %ebx,%edx
        subl    %edi,%edx       # length as startpos-curpos

        movl    %edi,%ecx       # pointer as startpos
        movl    $1,%ebx
        movl    $4,%eax
        int     $0x80

        /* Actually perform the test case */
        call    atoi            # read N to %eax
        movl    $1,%edi
        movb    %al,%cl
        shll    %cl,%edi        # %edi = 2^N
        decl    %edi            #        2^N - 1

        call    atoi            # read K to %eax

        movl    $off_s,%edx     # default to OFF
        movl    $off,%ecx
        
        andl    %edi,%eax       # %eax = K & (2^N-1)
        cmpl    %edi,%eax       # compare to 2^N - 1
	jne     print

        movl    $on_s,%edx
        movl    $on,%ecx

print:  movl    $1,%ebx
        movl    $4,%eax
        int     $0x80           # ON/OFF

        jmp     test            # and loop

atoi:   /*
           Convert ascii decimal at (%esi) to binary in %eax
           Accidentally preserves %edi
        */
        movl    $0,%eax
        movl    $0,%ebx
        movl    $10,%ecx
atoi_c: movb    (%esi),%bl      # read next char
        cmpb    $'0',%bl
        jb      atoi_e          # too low -> end
        cmpb    $'9',%bl
        ja      atoi_e          # too high -> end
        subl    $'0',%ebx
        mull    %ecx
        addl    %ebx,%eax       # merge into acc
        incl    %esi
        jmp     atoi_c
atoi_e: incl    %esi            # skip non-numeric
        ret 

.data

case:   .ascii "Case #"
        case_s = . - case

on:     .ascii ": ON\n"
        on_s = . - on

off:    .ascii ": OFF\n"
        off_s = . - off

.bss

        num_s = 12
num:    .space num_s
        
        buf_s = 6 + 10000 * 12
buf:    .space buf_s
