procedure SelectBank(Bank: Byte); register; external 'banksel_hl';

{$l derby.asm}
{$l overlays.asm}