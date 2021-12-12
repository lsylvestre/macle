#define MAP_HEAVY_INC16_CC_CTL 0
#define MAP_HEAVY_INC16_CC_A 1
#define MAP_HEAVY_INC16_CC_RESULT 2
#define MAP_HEAVY_INC16_CC_CAML_HEAP_BASE 3

int nios_map_heavy_inc16_cc(uint32_t a){
  alt_u32 result;
  int __dt = nios_timer_get_us();
  
  // Write arguments
  IOWR(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_A, a);
  IOWR(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_CAML_HEAP_BASE, ocaml_ram_heap);
  
  IOWR(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_CTL, 1);
  IOWR(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_CTL, 1); // to be improved
  
  while ( (result = IORD(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_CTL)) == 0 ); // Wait for rdy
  result = IORD(MAP_HEAVY_INC16_CC_BASE, MAP_HEAVY_INC16_CC_RESULT); // Read result
  
  __dt = nios_timer_get_us() - __dt;
  
  printf("\nellapsed time : %d us\n",__dt);
  
  return result;
}

