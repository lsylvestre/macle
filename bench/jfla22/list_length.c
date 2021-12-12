int c_len(uint32_t l){
  int __dt = nios_timer_get_us();
  value v = (value)l;
  int n = 0;
  while (v != Val_int(0)){ v = Field(v,1); ++n; }
  __dt = nios_timer_get_us() - __dt;
  printf("\nellapsed time : %d us\n",__dt);
  return n;
}

