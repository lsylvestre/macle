library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package misc_fact is
  type UNIT is (UNIT_VALUE);
  subtype caml_value is std_logic_vector(31 downto 0);
  subtype caml_ptr is std_logic_vector(31 downto 0);
  subtype caml_int is signed(30 downto 0);
  function bool_to_std_logic(X : boolean) return std_logic;
  function compute_address(heap_base : caml_value;
                           address : caml_value;
                           offset : caml_int) return caml_value;
  function size_header(x : caml_value) return caml_int;
  function tag_header(x : caml_value) return caml_int;
  function is_imm(x : caml_value) return boolean;
  
end;
package body misc_fact is
  function bool_to_std_logic(X : boolean) return std_logic is
    begin
      if X then
        return '1';
      else
        return '0';
      end if;
    end;
  function compute_address(heap_base : caml_value;
                           address : caml_value;
                           offset : caml_int) return caml_value is
    begin
      return std_logic_vector(
               unsigned(heap_base) +
               unsigned(address(19 downto 0)) +
               unsigned(offset(19 downto 0) & "00")
               );
    end;
  function size_header(x : caml_value) return caml_int is
    begin
      return signed("00000000000" & x(21 downto 2));
    end;
function tag_header(x : caml_value) return caml_int is
  begin
    return signed("00000000000000000000000" & x(31 downto 24));
  end;function is_imm(x : caml_value) return boolean is
begin
  return (x(0) = '1');end;end;

