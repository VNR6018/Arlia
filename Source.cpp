#include "System.hpp"
#include "AssemblerInsert.hpp"
#include "tokenizer.hpp"
#include "identifier.hpp"
#include "variables.hpp"

int main(int argc, char *argv[]) {
	System::Display::StartProgram("Funnel", "vnr", 00.01, "Arlia's compiler");
	std::string output = "FasmFileTest.asm";

	std::string RawCode = System::File::GetAllText(System::cpp::ArgumentManager<1>::get(argv));

	System::File::write(output, "; Code generated by the Funnel compiler (c) of the Arlia language (c).\n; This is currently a pre-alpha version, the output still requires optimizations / retouching.\n; Assembler language used : FASM.\n\n");
	AssemblerInsert::FinalCode code;
	/* ------- */

	/*tokenizer::tokenize tokens(RawCode);
	for (std::string str : tokens.GetLines())
		std::cout << str << std::endl;*/

	variable::List list;
	list.Append("foo", "integer", sizeof(int), "4");
	list.Append("foo", "integer", sizeof(int), "4");
	list.Append("foo", "integer", sizeof(int), "4");
	std::cout << list.size() << std::endl;

	/* ------- */
	System::File::WriteAppend(output, code.get());
	System::Display::ExitProgram();
	return 0;
}