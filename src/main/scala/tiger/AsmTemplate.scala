package tiger

/**
 * Created by jose on 16/01/15.
 */
object AsmTemplate {

  def header(filename:String) = s"\t.arch armv6" +
    "\t.fpu softvfp" +
    "\t.eabi_attribute 20, 1" +
    "\t.eabi_attribute 21, 1" +
    "\t.eabi_attribute 23, 3" +
    "\t.eabi_attribute 24, 1" +
    "\t.eabi_attribute 25, 1" +
    "\t.eabi_attribute 26, 2" +
    "\t.eabi_attribute 30, 6" +
    "\t.eabi_attribute 18, 4" +
    "\t.file\t\"${filename}.tig\"" +
    "\t.text"

  def functionHeader(funcname:String) = ""





}
