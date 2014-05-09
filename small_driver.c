#include <stdio.h>
#define fixnum_mask   0b11
#define fixnum_tag    0b00
#define fixnum_shift  2
#define empty_list    0b00101111
#define bool_t        0b10011111
#define bool_f        0b00011111
#define char_mask     0b11111111
#define char_tag      0b00001111
#define char_shift    8


int main(int argc, char** argv){
  int val = scheme_entry();
  if ((val & fixnum_mask) == fixnum_tag){
    printf("%d\n", val >> fixnum_shift);
  } else if (val == empty_list) {
    printf("()\n");
  } else if (val == bool_t) {
    printf("#t\n");
  } else if (val == bool_f) {
    printf("#f\n");
  } else if ((val & char_mask) == char_tag) {
    printf("#\\%c\n", val >> char_shift);
  }
  return 0;
}
