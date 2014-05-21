#include <stdio.h>
#include <stdlib.h>
#define fixnum_mask   0b11
#define fixnum_tag    0b00
#define fixnum_shift  2
#define empty_list    0b00101111
#define bool_t        0b10011111
#define bool_f        0b00011111
#define char_mask     0b11111111
#define char_tag      0b00001111
#define char_shift    8
#define pair_mask     0b111
#define pair_tag      0b001
#define INSIDE 1
#define OUTSIDE 0     // for toggling state in recursive calls to print_val

// long unsigned ints are 64 bits wide
// on 32 bit, we would use just unsigned ints
typedef long unsigned int ptr;

typedef struct {
  ptr car;
  ptr cdr;
} pair;

int print_val(ptr val, int state){

  if ((val & fixnum_mask) == fixnum_tag){
    printf("%d", (int)(val >> fixnum_shift));
  } else if (val == empty_list) {
    printf("()");
  } else if (val == bool_t) {
    printf("#t");
  } else if (val == bool_f) {
    printf("#f");
  } else if ((val & char_mask) == char_tag) {
    printf("#\\%c", (char)(val >> char_shift));
  } else if ((val & pair_mask) == pair_tag) {
    if (state == OUTSIDE) printf("("); // to start with

    ptr car = ((pair*)(val - 1))->car; // this is magic - we are pointer walking by 
    ptr cdr = ((pair*)(val - 1))->cdr; // virtue of the alignment of structs in memory

    //ptr car = (ptr)(ptr*)(val - 1);
    //ptr cdr = (ptr)(ptr*)(val + 7);
    //printf("val: %lu, car: %lu, cdr: %lu\n", (ptr)val, (ptr)car, (ptr)cdr);
    //printf("val: %p, car: %p, cdr: %p\n", (void*)val, (void*)car, (void*)cdr);

    print_val(car, OUTSIDE); // TODO check for fails
    if (cdr != empty_list){
      if ((cdr & pair_mask) == pair_tag) {
        printf(" ");
        print_val(cdr, INSIDE);
      } else {
        printf(" . ");
        print_val(cdr, OUTSIDE);
      }
    }
    if (state == OUTSIDE) printf(")");
  } else {
    printf("unknown value returned: %lu\n", val);
    return 1;
  }
  return 0;
}

int main(int argc, char** argv){
  
  // from http://www.delorie.com/gnu/docs/glibc/libc_31.html
  // this should return a 16-bit aligned address on x64
  // This is not clear from the man page, so in case we need
  // it later, this is a reminder to look at posix_memalign(3)
  ptr* heap = (ptr *)calloc(1024, (sizeof(ptr)));

  ptr val = scheme_entry(heap);
  int ret = print_val(val, OUTSIDE);
  printf("\n");
  return ret;
}
