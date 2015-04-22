
import dis

from .instruction import StackInstruction

class Function(object):
    def __init__(self, python_function):
        current_block_instructions = []
        for simple_instruction in dis.Bytecode(python_function):
            opcode = simple_instruction.opcode
            instruction_class = StackInstruction.from_opcode(opcode)
            instruction = instruction_class(
                offset=simple_instruction.offset,
                arg=simple_instruction.arg,
                is_jump_target=simple_instruction.is_jump_target)
            current_block_instructions.append(instruction)
