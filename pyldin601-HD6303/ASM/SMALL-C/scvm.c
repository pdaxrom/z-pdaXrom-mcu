#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#define byte unsigned char
#define word unsigned short

#define LD1IM		2*0
#define LD1SOFF		2*1
#define LD1		2*2
#define LDB1		2*3
#define LD1R		2*4
#define LDB1R		2*5
#define ST1		2*6
#define STB1		2*7
#define ST1SP		2*8
#define STB1SP		2*9
#define PUSHR1		2*10
#define EXG1		2*11
#define JMPL		2*12
#define BRZL		2*13
#define JSRL		2*14
#define JSRSP		2*15
#define RTSC		2*16
#define MODSP		2*17
#define DBL1		2*18
#define ADDS		2*19
#define SUBFST		2*20
#define MUL1		2*21
#define DIV1		2*22
#define MOD		2*23
#define ORS		2*24
#define XORS		2*25
#define ANDS		2*26
#define ASRS		2*27
#define ASLS		2*28
#define NEGR		2*29
#define NOTR		2*30
#define INCR		2*31
#define DECR		2*32
#define ZEQ		2*33
#define ZNE		2*34
#define ZLT		2*35
#define ZLE		2*36
#define ZGT		2*37
#define ZGE		2*38
#define ULT		2*39
#define ULE		2*40
#define UGT		2*41
#define UGE		2*42
#define ASMC		2*43

#define f_fclose	0x104
#define f_fopen		0x108
#define f_getc		0x10C
#define f_getchar	0x110
#define f_gets		0x114
#define f_putc		0x118
#define f_putchar	0x11C
#define f_puts		0x120
#define f_RTSC		0x124
#define f_isalpha	0x128
#define f_isdigit	0x12C
#define f_isalnum	0x130
#define f_islower	0x134
#define f_isupper	0x138
#define f_isspace	0x13C
#define f_toupper	0x140
#define f_tolower	0x144
#define f_strclr	0x148
#define f_strlen	0x14C
#define f_strcpy	0x150
#define f_strcat	0x154
#define f_strcmp	0x158
#define f_exit		0x15C

byte mem[0x10000];
word start = 0x800;

void chkfunc(word *sp, word *pc, word *reg)
{
    if (*pc < start) {
	word tmp, tmp1;
	byte c;
//	fprintf(stderr, "External function request, PC=$%04X\n", *pc);

	switch (*pc) {
	case f_fclose: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; close(tmp); *reg = 0; break;
	case f_fopen:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		FILE *f = fopen((char *)&mem[tmp], (char *)&mem[tmp1]);
		*reg = f ? fileno(f) : 0;
		break;
	case f_getc: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; tmp1 = read(tmp, &c, 1); *reg = (tmp1 == 1) ? c : -1; break;
	case f_getchar: *reg = getchar(); break;
	case f_gets: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = gets(&mem[tmp]) ? tmp : 0; break;
	case f_putc:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		c = tmp;
		tmp1 = write(tmp1, &c, 1);
		*reg = (tmp1 == 1) ? c : -1;
		break;
	case f_putchar: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; printf("%c", tmp); *reg = tmp; break;
	case f_puts: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; printf("%s", &mem[tmp]); *reg = 0; break;

	case f_isalpha: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = isalpha(tmp); break;
	case f_isdigit: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = isdigit(tmp); break;
	case f_isalnum: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = isalnum(tmp); break;
	case f_islower: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = islower(tmp); break;
	case f_isupper: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = isupper(tmp); break;
	case f_isspace: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = isspace(tmp); break;
	case f_toupper: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = toupper(tmp); break;
	case f_tolower: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = tolower(tmp); break;
	case f_strclr:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		memset(&mem[tmp], 0, tmp1);
		*reg = tmp;
		break;
	case f_strlen: tmp = (mem[*sp + 3] << 8) | mem[*sp + 4]; *reg = strlen((char *)&mem[tmp]); break;
	case f_strcpy:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		strcpy((char *)&mem[tmp], (char *)&mem[tmp1]);
		*reg = tmp;
		break;
	case f_strcat:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		strcat((char *)&mem[tmp], (char *)&mem[tmp1]);
		*reg = tmp;
		break;
	case f_strcmp:
		tmp1 = (mem[*sp + 3] << 8) | mem[*sp + 4];
		tmp = (mem[*sp + 5] << 8) | mem[*sp + 6];
		*reg = strcmp((char *)&mem[tmp], (char *)&mem[tmp1]);
		break;

	case f_exit: exit(0); break;
	default: fprintf(stderr, "Unimplemented function %X\n", *pc); exit(1); break;
	}

	*pc = (mem[*sp + 1] << 8) | mem[*sp + 2];
	*sp = *sp + 2;
    }
}

int main(int argc, char *argv[])
{
    FILE *inf;
    word len;

    word sp;
    word pc;

    fprintf(stderr, "SmallC virtual machine\n");

    inf = fopen(argv[1], "rb");
    if (!inf) {
	fprintf(stderr, "Can't open file %s\n", argv[1]);
	return 1;
    }
    len = fread(&mem[start], 1, sizeof(mem) - start, inf);
    fclose(inf);

    sp = 0xFF00;
    pc = 0xFF20;
    for (int i = 1; i < argc; i++) {
	strcpy(&mem[pc], argv[i]);
	mem[sp++] = pc >> 8;
	mem[sp++] = pc & 0xFF;
	pc += strlen(argv[i]) + 1;
    }

/*
    pc = 0xFF00;
    for (int i = 0; i < argc - 1; i++) {
	printf("arg%d [%s]\n", i, &mem[(mem[pc] << 8) | mem[pc + 1]]);
	pc += 2;
    }
 */

    argc--;
    sp = 0xFEFF;
    mem[sp--] = argc & 0xFF;
    mem[sp--] = argc >> 8;
    mem[sp--] = 0x00;
    mem[sp--] = 0xFF;

    pc = start;
//    sp = 0xFEFF;	// maximum memory
    word reg = 0;
    word tmp = 0;

    int running = 1;

    while (running) {
	switch (mem[pc++]) {
	case LD1IM:	reg = (mem[pc] << 8) | mem[pc + 1]; pc += 2; break;
	case LD1SOFF:	reg = (mem[pc] << 8) | mem[pc + 1]; pc += 2; reg += sp + 1; break;
	case LD1:	tmp = (mem[pc] << 8) | mem[pc + 1]; pc += 2; reg = (mem[tmp] << 8) | mem[tmp + 1]; break;
	case LDB1:	tmp = (mem[pc] << 8) | mem[pc + 1]; pc += 2; reg = (((mem[tmp] & 0x80)? 0xFF : 0) << 8) | mem[tmp]; break;
	case LD1R:	reg = (mem[reg] << 8) | mem[reg + 1]; break;
	case LDB1R:	reg = (((mem[reg] & 0x80)? 0xFF : 0) << 8) | mem[reg]; break;
	case ST1:	tmp = (mem[pc] << 8) | mem[pc + 1]; pc += 2; mem[tmp] = reg >> 8; mem[tmp + 1] = reg & 0xFF; break;
	case STB1:	tmp = (mem[pc] << 8) | mem[pc + 1]; pc += 2; mem[tmp] = reg & 0xFF; break;
	case ST1SP:	tmp = sp + 1; tmp = (mem[tmp] << 8) | mem[tmp + 1]; mem[tmp] = reg >> 8; mem[tmp + 1] = reg & 0xFF; sp += 2; break;
	case STB1SP:	tmp = sp + 1; tmp = (mem[tmp] << 8) | mem[tmp + 1]; mem[tmp] = reg & 0xFF; sp += 2; break;
	case PUSHR1:	mem[sp--] = reg & 0xFF; mem[sp--] = reg >> 8; break;
	case EXG1:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; mem[sp + 1] = reg >> 8; mem[sp + 2] = reg & 0xFF; reg = tmp; break;
	case JMPL:	pc = (mem[pc] << 8) | mem[pc + 1]; break;
	case BRZL:	if (reg == 0) { pc = (mem[pc] << 8) | mem[pc + 1]; } else { pc += 2; }; break;
	case JSRL:	tmp = pc + 2; mem[sp--] = tmp & 0xFF; mem[sp--] = tmp >> 8; pc = (mem[pc] << 8) | mem[pc + 1]; chkfunc(&sp, &pc, &reg); break;
	case JSRSP:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; mem[sp + 1] = pc >> 8; mem[sp + 2] = pc & 0xFF; pc = tmp; chkfunc(&sp, &pc, &reg); break;
	case RTSC:	pc = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; break;
	case MODSP:	tmp = (mem[pc] << 8) | mem[pc + 1]; pc += 2; sp += tmp; break;
	case DBL1:	reg <<= 1; break;
	case ADDS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp + reg; break;
	case SUBFST:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp - reg; break;
	case MUL1:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp * reg; break;
	case DIV1:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp / reg; break;
	case MOD:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp % reg; break;
	case ORS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp | reg; break;
	case XORS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp ^ reg; break;
	case ANDS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp & reg; break;
	case ASRS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp >> reg; break;
	case ASLS:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = tmp << reg; break;
	case NEGR:	reg = ~reg; reg++; break;
	case NOTR:	reg = ~reg; break;
	case INCR:	reg++; break;
	case DECR:	reg--; break;
	case ZEQ:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp == reg) ? 1: 0; break;
	case ZNE:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp != reg) ? 1: 0; break;
	case ZLT:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (((short)tmp) < ((short)reg)) ? 1: 0; break;
	case ZLE:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (((short)tmp) <= ((short)reg)) ? 1: 0; break;
	case ZGT:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (((short)tmp) > ((short)reg)) ? 1: 0; break;
	case ZGE:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (((short)tmp) >= ((short)reg)) ? 1: 0; break;
	case ULT:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp < reg) ? 1: 0; break;
	case ULE:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp <= reg) ? 1: 0; break;
	case UGT:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp > reg) ? 1: 0; break;
	case UGE:	tmp = (mem[sp + 1] << 8) | mem[sp + 2]; sp += 2; reg = (tmp >= reg) ? 1: 0; break;
	case ASMC:	if (!((mem[pc] == 0x3f) && mem[pc + 1] == 0x38)) fprintf(stderr, "ASMC is not supported!"); running = 0; break;
	default:	fprintf(stderr, "Unsupported bytecode %02X at %04X\n", mem[pc - 1], pc - 1); running = 0; break;
	}
    }

    return 0;
}