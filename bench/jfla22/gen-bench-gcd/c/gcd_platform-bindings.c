uint32_t caml_nios_gcd_cc(uint32_t a,uint32_t b) {
  return Val_int(nios_gcd_cc(Int_val(a),Int_val(b)));
}
