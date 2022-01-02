uint32_t caml_nios_fact_cc(uint32_t n) {
  return Val_int(nios_fact_cc(Int_val(n)));
}
