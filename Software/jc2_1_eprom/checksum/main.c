/* This program calculates the checksum for a JC2 .bin file. */
/* Memory area to check: $B000-$FFFF. ROM checksum at        */
/* $DFF0 and $DFF1 is excluded from this checksum.           */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

// Monitor starts at 0x1C00, MON_CS_IN_ROM is at 0x7FF4
// BASIC   starts at 0x3000, BAS_CS_IN_ROM is at 0x7FF6
// BIOS    starts at 0x5000, ROM_CS_IN_ROM is at 0x7FF8
#define ROM_SIZE      (0x8000)   /* JC2 BIOS eprom is 32K */
#define MON_CS_IN_ROM (0x7FF4)   /* $FFF4-$FFF5 */
#define BAS_CS_IN_ROM (0x7FF6)   /* $FFF6-$FFF7 */
#define ROM_CS_IN_ROM (0x7FF8)   /* $FFF8-$FFF9 */

uint8_t  rom[ROM_SIZE];

uint16_t calc_checksum(uint16_t begin, uint16_t end)
{
    uint16_t checksum = 0;
    uint16_t i;

    for (i = begin; i < end; i++)
    {
        checksum += rom[i];
    } // for i
    return checksum;
} // calc_checksum()

int main(int argc, char *argv[])
{
	FILE     *f;
	uint16_t bytes; // #bytes read from file
	uint16_t mon_cs = 0;
	uint16_t bas_cs = 0;
	uint16_t rom_cs = 0;
    uint16_t rom_cs_in_rom;
    uint16_t bas_cs_in_rom;
    uint16_t mon_cs_in_rom;

	if (argc != 2)
    {
		fprintf(stderr, " Usage: %s <input filename>\n", argv[0]);
		return EXIT_FAILURE;
	} // if

	/* Read ROM image. */
	f = fopen(argv[1], "rb");
	if (f == NULL)
    {
		fprintf(stderr, "Can't open %s\n", argv[1]);
		return EXIT_FAILURE;
	} // if

	bytes = fread(rom, sizeof(uint8_t), ROM_SIZE, f);
    printf("$%04X bytes read from %s",bytes,argv[1]);

    if (bytes == 32768)
    {   // 32K eprom
        printf(", EPROM Size ok!\n");

        mon_cs = calc_checksum(0x1C00,0x2000);
        bas_cs = calc_checksum(0x3000,0x6000);
        rom_cs = calc_checksum(0x6000,0x7FF0);
        printf("Monitor ROM checksum = $%04X\n",mon_cs);
        printf("BASIC   ROM checksum = $%04X\n",bas_cs);
        printf("BIOS    ROM checksum = $%04X\n",rom_cs);
        fclose(f);
    } // if
    else
    {
        printf(", EPROM Size error, should be 32K!\n");
        fclose(f);
        return EXIT_FAILURE;
    } // else

	f = fopen(argv[1], "wb");
	if (f == NULL)
    {
		fprintf(stderr, "Can't open %s for writing\n", argv[1]);
		return EXIT_FAILURE;
	} // if
    else
    {
        mon_cs_in_rom = rom[MON_CS_IN_ROM] + 256*rom[MON_CS_IN_ROM+1];
        bas_cs_in_rom = rom[BAS_CS_IN_ROM] + 256*rom[BAS_CS_IN_ROM+1];
        rom_cs_in_rom = rom[ROM_CS_IN_ROM] + 256*rom[ROM_CS_IN_ROM+1];
        if (mon_cs_in_rom == mon_cs)
        {
            printf("Monitor checksum matches, no patching needed\n");
        } // if
        else
        {
            printf("Now patching %s with Monitor ROM checksum: ",argv[1]);
            rom[MON_CS_IN_ROM+1] = (uint8_t)(mon_cs / 256);
            rom[MON_CS_IN_ROM]   = (uint8_t)(mon_cs - 256 * rom[MON_CS_IN_ROM+1]);
            printf("$%04X($FFF4):%02X, $%04X($FFF5):%02X\n",MON_CS_IN_ROM,rom[MON_CS_IN_ROM],MON_CS_IN_ROM+1,rom[MON_CS_IN_ROM+1]);
        } // else
        if (bas_cs_in_rom == bas_cs)
        {
            printf("BASIC checksum matches, no patching needed\n");
        } // if
        else
        {
            printf("Now patching %s with BASIC ROM checksum  : ",argv[1]);
            rom[BAS_CS_IN_ROM+1] = (uint8_t)(bas_cs / 256);
            rom[BAS_CS_IN_ROM]   = (uint8_t)(bas_cs - 256 * rom[BAS_CS_IN_ROM+1]);
            printf("$%04X($FFF6):%02X, $%04X($FFF7):%02X\n",BAS_CS_IN_ROM,rom[BAS_CS_IN_ROM],BAS_CS_IN_ROM+1,rom[BAS_CS_IN_ROM+1]);
        } // else
        if (rom_cs_in_rom == rom_cs)
        {
            printf("BIOS checksum matches, no patching needed\n");
        } // if
        else
        {
            printf("Now patching %s with BIOS ROM checksum   : ",argv[1]);
            rom[ROM_CS_IN_ROM+1] = (uint8_t)(rom_cs / 256);
            rom[ROM_CS_IN_ROM]   = (uint8_t)(rom_cs - 256 * rom[MON_CS_IN_ROM+1]);
            printf("$%04X($FFF8):%02X, $%04X($FFF9):%02X\n",ROM_CS_IN_ROM,rom[ROM_CS_IN_ROM],ROM_CS_IN_ROM+1,rom[ROM_CS_IN_ROM+1]);
        } // else
        fwrite(rom,sizeof(uint8_t), bytes, f); // always write, otherwise file is empty
    } // else
	if (fclose(f) != 0)
    {
		fprintf(stderr, "Can't close %s\n", argv[1]);
		return EXIT_FAILURE;
	} // if
	return EXIT_SUCCESS;
} // main()
