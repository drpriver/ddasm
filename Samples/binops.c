#pragma library("libc")
int printf(const char*, ...);
void start(){
  const char* fmt = "%-16s | %zd\n";
  printf(fmt, "1 + 2", 1 + 2);
  printf(fmt, "8 - 3", 8 - 3);
  printf(fmt, "3 * 3", 3 * 3);
  printf(fmt, "8 / 2", 8 / 2);
  printf(fmt, "8 % 3", 8 % 3);
  printf(fmt, "1 < 2", 1 < 2);
  printf(fmt, "1 > 2", 1 > 2);
  printf(fmt, "1 <= 2", 1 <= 2);
  printf(fmt, "1 >= 2", 1 >= 2);
  printf(fmt, "1 == 2", 1 == 2);
  printf(fmt, "1 != 2", 1 != 2);
  fmt = "%-16s | 0x%.2zx\n";
  printf(fmt, "0xff & 0xff", 0xff & 0x0f);
  printf(fmt, "0xf0 | 0x0f", 0xf0 | 0x0f);
  printf(fmt, "0b1010 ^ 0b0011", 0b1010 ^ 0b0011);
   printf(fmt, "0xf0 && 0x0f", 0xf0 && 0x0f);
   printf(fmt, "0 || 1", 0 || 1);
   printf(fmt, "0 || 0", 0 || 0);
  printf(fmt, "-1 << 56", -1l << 56);
  printf(fmt, "-1 >> 56", -1l >> 56);
  printf(fmt, "(-1 << 16) >> 56", (-1l << 16) >> 56);
}
