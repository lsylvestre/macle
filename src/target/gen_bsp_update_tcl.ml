open Format

let gen_bsp_update_tcl ?(heap=false) ?(rodata=false)
      ?(stack=false) ?(rwdata=false) ?(bss=false) fmt =
  
  let put_on_chip_if b =
    if b then "onchip_memory" else "sdram" 
  in
  
  fprintf fmt "@[<v>update_section_mapping .heap %s@," (put_on_chip_if heap);
  fprintf fmt "update_section_mapping .rodata %s@," (put_on_chip_if rodata);
  fprintf fmt "update_section_mapping .stack %s@,"  (put_on_chip_if stack);
  fprintf fmt "update_section_mapping .rwdata %s@," (put_on_chip_if rwdata);
  fprintf fmt "update_section_mapping .bss %s@,@]"  (put_on_chip_if bss)
