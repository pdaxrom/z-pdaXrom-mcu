/*
 *
 * Usage:
 *   gcc -DXBMFILE=\"flowers-rose.xbm\" genpix.c -o genpix
 *
 * (c) sashz <sashz@pdaXrom.org> 2017-2018
 *
 */

#include XBMFILE
#include <stdio.h>

unsigned char revb(unsigned char in)
{
    int i;
    unsigned char ret = 0;
    for (i = 0; i < 8; i++) {
	ret >>= 1;
	ret |= in & 0x80;
	in <<= 1;
    }
    return ret;
}

int main(int argc, char *argv[])
{
    int i = 0;

    for (i = 0; i < (_width / 8) * _height; i++) {
	if (i % (_width / 8) == 0) printf("\n\tdb ");
	printf("$%02X", revb(_bits[i]));
	if (i % (_width / 8) != (_width / 8 - 1)) printf(", ");
    }

    printf("\n");

    return 0;
}
