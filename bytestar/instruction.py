from collections import OrderedDict
from opcode import (
    # list of which opcodes jump to relative offset
    hasjrel as relative_jump_opcodes,
    # list of which opcodes jump to absolute offset
    hasjabs as absolute_jump_opcodes,
    # dictionary mapping operator names to bytecodes
    opmap as opname_to_opcode_dict,
    # list of opcode names
    opname as opcode_to_opname_list,

)

jump_opcodes = set(relative_jump_opcodes + absolute_jump_opcodes)
jump_opnames = {opcode_to_opname_list[opcode] for opcode in jump_opcodes}

class InstructionMeta(type):
    """
    Meta-class for instructions which we use to register derived classes
    """
    def __init__(cls, name, bases, dct):
        if not hasattr(cls, 'registry'):
            # this is the base class.  Create an empty registry
            cls.registry = {}
        else:
            # this is a derived class.  Add cls to the registry
            cls.registry[name] = cls
        super(InstructionMeta, cls).__init__(name, bases, dct)

class StackInstruction(object):
    __metaclass__ = InstructionMeta

    def __init__(
            self,
            offset,
            arg=None,
            is_jump_target=False):
        self.offset = offset
        self.arg = arg
        self.is_jump_target = is_jump_target
        self.is_jump = self.opcode in jump_opcodes

    def __str__(self):
        args = OrderedDict(offset=self.offset)
        if self.arg is not None:
            args["arg"] = self.arg
        if self.is_jump_target:
            args["is_jump_target"] = True
        args_str = ", ".join(
            "%s=%s" % (k, v) for (k, v) in args.items())
        return "%s(%s)" % (self.opname, args_str)

    def __repr__(self):
        return str(self)

    @classmethod
    def from_opname(cls, opname):
        return cls.registry[opname]

    @classmethod
    def from_opcode(cls, opcode):
        return cls.from_opname[opname_to_opcode_dict[opcode]]

    @property
    def opname(self):
        return self.__class__.__name__

    @property
    def opcode(self):
        return opname_to_opcode_dict[self.opname]

    # number of items removed from stack as inputs to the instruction
    n_pop = 0
    # number of items added back to the stack as outputs of the instruction
    n_push = 0


class NOP(StackInstruction):
    """Do nothing code. Used as a placeholder by the bytecode optimizer."""
    pass

class POP_TOP(StackInstruction):
    """Removes the top-of-stack (TOS) item."""
    n_pop = 1


class ROT_TWO(StackInstruction):
    """Swaps the two top-most stack items."""
    n_pop = 2
    n_push = 2

class ROT_THREE(StackInstruction):
    """Lifts second and third stack item one position up, moves top down to
    position three."""
    n_pop = 3
    n_push = 3

class DUP_TOP(StackInstruction):
    """Duplicates the reference on top of the stack."""
    n_pop = 1
    n_push = 2


class DUP_TOP_TWO(StackInstruction):
    """Duplicates the two references on top of the stack,
    leaving them in the same order."""
    n_pop = 2
    n_push = 4


class UnaryStackInstruction(StackInstruction):
    """Unary operations take the top of the stack, apply the operation,
    and push the result back on the stack."""
    n_pop = 1
    n_push = 1

class UNARY_POSITIVE(UnaryStackInstruction):
    """Implements TOS = +TOS."""
    pass

class UNARY_NEGATIVE(UnaryStackInstruction):
    """Implements TOS = -TOS."""
    pass

class UNARY_NOT(UnaryStackInstruction):
    """Implements TOS = not TOS."""
    pass

class UNARY_INVERT(UnaryStackInstruction):
    """Implements TOS = ~TOS."""
    pass

class GET_ITER(UnaryStackInstruction):
    """Implements TOS = iter(TOS)."""
    pass

class BinaryStackInstruction(StackInstruction):
    """Binary operations remove the top of the stack (TOS) and the second
    top-most stack item (TOS1) from the stack. They perform the operation, and
    put the result back on the stack.
    """
    n_pop = 2
    n_push = 1

class BINARY_POWER(BinaryStackInstruction):
    """Implements TOS = TOS1 ** TOS."""
    pass

class BINARY_MULTIPLY(BinaryStackInstruction):
    """Implements TOS = TOS1 * TOS."""
    pass

class BINARY_FLOOR_DIVIDE(BinaryStackInstruction):
    """Implements TOS = TOS1 // TOS."""
    pass

class BINARY_TRUE_DIVIDE(BinaryStackInstruction):
    """Implements TOS = TOS1 / TOS."""
    pass

class BINARY_MODULO(BinaryStackInstruction):
    """Implements TOS = TOS1 % TOS."""
    pass

class BINARY_ADD(BinaryStackInstruction):
    """Implements TOS = TOS1 + TOS."""
    pass

class BINARY_SUBTRACT(BinaryStackInstruction):
    """Implements TOS = TOS1 - TOS."""
    pass

class BINARY_SUBSCR(BinaryStackInstruction):
    """Implements TOS = TOS1[TOS]."""
    pass

class BINARY_LSHIFT(BinaryStackInstruction):
    """Implements TOS = TOS1 << TOS."""
    pass

class BINARY_RSHIFT(BinaryStackInstruction):
    """Implements TOS = TOS1 >> TOS."""
    pass

class BINARY_AND(BinaryStackInstruction):
    """Implements TOS = TOS1 & TOS."""
    pass

class BINARY_XOR(BinaryStackInstruction):
    """Implements TOS = TOS1 ^ TOS."""
    pass

class BINARY_OR(BinaryStackInstruction):
    """Implements TOS = TOS1 | TOS."""
    pass

class InplaceStackInstruction(StackInstruction):
    n_pop = 2
    n_push = 1

class INPLACE_POWER(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 ** TOS."""

class INPLACE_MULTIPLY(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 * TOS."""

class INPLACE_FLOOR_DIVIDE(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 // TOS."""

class INPLACE_TRUE_DIVIDE(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 / TOS."""

class INPLACE_MODULO(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 % TOS."""

class INPLACE_ADD(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 + TOS."""

class INPLACE_SUBTRACT(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 - TOS."""

class INPLACE_LSHIFT(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 << TOS."""

class INPLACE_RSHIFT(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 >> TOS."""

class INPLACE_AND(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 & TOS."""

class INPLACE_XOR(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 ^ TOS."""

class INPLACE_OR(InplaceStackInstruction):
    """Implements in-place TOS = TOS1 | TOS."""

class STORE_SUBSCR(InplaceStackInstruction):
    """Implements TOS1[TOS] = TOS2."""

class DELETE_SUBSCR(InplaceStackInstruction):
    """Implements del TOS1[TOS]."""

# TODO: implement remaining instructions:
"""
PRINT_EXPR
Implements the expression statement for the interactive mode. TOS is removed from the stack and printed. In non-interactive mode, an expression statement is terminated with POP_TOP.

BREAK_LOOP
Terminates a loop due to a break statement.

CONTINUE_LOOP(target)
Continues a loop due to a continue statement. target is the address to jump to (which should be a FOR_ITER instruction).

SET_ADD(i)
Calls set.add(TOS1[-i], TOS). Used to implement set comprehensions.

LIST_APPEND(i)
Calls list.append(TOS[-i], TOS). Used to implement list comprehensions.

MAP_ADD(i)
Calls dict.setitem(TOS1[-i], TOS, TOS1). Used to implement dict comprehensions.

For all of the SET_ADD, LIST_APPEND and MAP_ADD instructions, while the added value or key/value pair is popped off, the container object remains on the stack so that it is available for further iterations of the loop.

RETURN_VALUE
Returns with TOS to the caller of the function.

YIELD_VALUE
Pops TOS and yields it from a generator.

YIELD_FROM
Pops TOS and delegates to it as a subiterator from a generator.

New in version 3.3.

IMPORT_STAR
Loads all symbols not starting with '_' directly from the module TOS to the
local namespace. The module is popped after loading all names. This opcode
implements from module import *.

POP_BLOCK
Removes one block from the block stack. Per frame, there is a stack of blocks,
denoting nested loops, try statements, and such.

POP_EXCEPT
Removes one block from the block stack. The popped block must be an exception
handler block, as implicitly created when entering an except handler. In
addition to popping extraneous values from the frame stack, the last three
popped values are used to restore the exception state.

END_FINALLY
Terminates a finally clause. The interpreter recalls whether the exception has
to be re-raised, or whether the function returns, and continues with the
outer-next block.

LOAD_BUILD_CLASS
Pushes builtins.__build_class__() onto the stack. It is later called by
CALL_FUNCTION to construct a class.

SETUP_WITH(delta)
This opcode performs several operations before a with block starts. First, it
loads __exit__() from the context manager and pushes it onto the stack for
later use by WITH_CLEANUP. Then, __enter__() is called, and a finally block
pointing to delta is pushed. Finally, the result of calling the enter method is
pushed onto the stack. The next opcode will either ignore it (POP_TOP), or
store it in (a) variable(s) (STORE_FAST, STORE_NAME, or UNPACK_SEQUENCE).

WITH_CLEANUP
Cleans up the stack when a with statement block exits. TOS is the context
manager’s __exit__() bound method. Below TOS are 1–3 values indicating how/why
the finally clause was entered:

SECOND = None
(SECOND, THIRD) = (WHY_{RETURN,CONTINUE}), retval
SECOND = WHY_*; no retval below it
(SECOND, THIRD, FOURTH) = exc_info()
In the last case, TOS(SECOND, THIRD, FOURTH) is called,
otherwise TOS(None, None, None). In addition, TOS is removed from the stack.

If the stack represents an exception, and the function call returns a ‘true’
value, this information is “zapped” and replaced with a single WHY_SILENCED to
prevent END_FINALLY from re-raising the exception. (But non-local gotos will
    still be resumed.)

All of the following opcodes expect arguments. An argument is two bytes, with
the more significant byte last.

STORE_NAME(namei)
Implements name = TOS. namei is the index of name in the attribute co_names of
the code object. The compiler tries to use STORE_FAST or STORE_GLOBAL if
possible.

DELETE_NAME(namei)
Implements del name, where namei is the index into co_names attribute of the
code object.

UNPACK_SEQUENCE(count)
Unpacks TOS into count individual values, which are put onto the stack
right-to-left.

UNPACK_EX(counts)
Implements assignment with a starred target: Unpacks an iterable in TOS into
individual values, where the total number of values can be smaller than the
number of items in the iterable: one the new values will be a list of all
leftover items.

The low byte of counts is the number of values before the list value, the high
byte of counts the number of values after it. The resulting values are put onto
the stack right-to-left.

STORE_ATTR(namei)
Implements TOS.name = TOS1, where namei is the index of name in co_names.

DELETE_ATTR(namei)
Implements del TOS.name, using namei as index into co_names.

STORE_GLOBAL(namei)
Works as STORE_NAME, but stores the name as a global.

DELETE_GLOBAL(namei)
Works as DELETE_NAME, but deletes a global name.

LOAD_CONST(consti)
Pushes co_consts[consti] onto the stack.

LOAD_NAME(namei)
Pushes the value associated with co_names[namei] onto the stack.

BUILD_TUPLE(count)
Creates a tuple consuming count items from the stack, and pushes the resulting
tuple onto the stack.

BUILD_LIST(count)
Works as BUILD_TUPLE, but creates a list.

BUILD_SET(count)
Works as BUILD_TUPLE, but creates a set.

BUILD_MAP(count)
Pushes a new dictionary object onto the stack. The dictionary is pre-sized to
hold count entries.

LOAD_ATTR(namei)
Replaces TOS with getattr(TOS, co_names[namei]).

COMPARE_OP(opname)
Performs a Boolean operation. The operation name can be found in cmp_op[opname].

IMPORT_NAME(namei)
Imports the module co_names[namei]. TOS and TOS1 are popped and provide the
fromlist and level arguments of __import__(). The module object is pushed onto
the stack. The current namespace is not affected: for a proper import statement,
a subsequent STORE_FAST instruction modifies the namespace.

IMPORT_FROM(namei)
Loads the attribute co_names[namei] from the module found in TOS. The resulting
object is pushed onto the stack, to be subsequently stored by a STORE_FAST
instruction.

JUMP_FORWARD(delta)
Increments bytecode counter by delta.

POP_JUMP_IF_TRUE(target)
If TOS is true, sets the bytecode counter to target. TOS is popped.

POP_JUMP_IF_FALSE(target)
If TOS is false, sets the bytecode counter to target. TOS is popped.

JUMP_IF_TRUE_OR_POP(target)
If TOS is true, sets the bytecode counter to target and leaves TOS on the stack.
Otherwise (TOS is false), TOS is popped.

JUMP_IF_FALSE_OR_POP(target)
If TOS is false, sets the bytecode counter to target and leaves TOS on the
stack. Otherwise (TOS is true), TOS is popped.

JUMP_ABSOLUTE(target)
Set bytecode counter to target.

FOR_ITER(delta)
TOS is an iterator. Call its __next__() method. If this yields a new value,
push it on the stack (leaving the iterator below it). If the iterator indicates
it is exhausted TOS is popped, and the byte code counter is incremented by
delta.

LOAD_GLOBAL(namei)
Loads the global named co_names[namei] onto the stack.

SETUP_LOOP(delta)
Pushes a block for a loop onto the block stack. The block spans from the current
instruction with a size of delta bytes.

SETUP_EXCEPT(delta)
Pushes a try block from a try-except clause onto the block stack. delta
points to the first except block.

SETUP_FINALLY(delta)
Pushes a try block from a try-except clause onto the block stack. delta points
to the finally block.

STORE_MAP
Store a key and value pair in a dictionary. Pops the key and value while leaving
the dictionary on the stack.

LOAD_FAST(var_num)
Pushes a reference to the local co_varnames[var_num] onto the stack.

STORE_FAST(var_num)
Stores TOS into the local co_varnames[var_num].

DELETE_FAST(var_num)
Deletes local co_varnames[var_num].

LOAD_CLOSURE(i)
Pushes a reference to the cell contained in slot i of the cell and free variable
storage. The name of the variable is co_cellvars[i] if i is less than the length
of co_cellvars. Otherwise it is co_freevars[i - len(co_cellvars)].

LOAD_DEREF(i)
Loads the cell contained in slot i of the cell and free variable storage.
Pushes a reference to the object the cell contains on the stack.

LOAD_CLASSDEREF(i)
Much like LOAD_DEREF but first checks the locals dictionary before consulting
the cell. This is used for loading free variables in class bodies.

STORE_DEREF(i)
Stores TOS into the cell contained in slot i of the cell and free variable
storage.

DELETE_DEREF(i)
Empties the cell contained in slot i of the cell and free variable storage.
Used by the del statement.

RAISE_VARARGS(argc)
Raises an exception. argc indicates the number of parameters to the raise
statement, ranging from 0 to 3. The handler will find the traceback as TOS2,
the parameter as TOS1, and the exception as TOS.

CALL_FUNCTION(argc)
Calls a function. The low byte of argc indicates the number of positional
parameters, the high byte the number of keyword parameters. On the stack, the
opcode finds the keyword parameters first. For each keyword argument, the value
is on top of the key. Below the keyword parameters, the positional parameters
are on the stack, with the right-most parameter on top. Below the parameters,
the function object to call is on the stack. Pops all function arguments, and
the function itself off the stack, and pushes the return value.

MAKE_FUNCTION(argc)
Pushes a new function object on the stack. From bottom to top, the consumed
stack must consist of

argc & 0xFF default argument objects in positional order
(argc >> 8) & 0xFF pairs of name and default argument, with the name just below
the object on the stack, for keyword-only parameters
(argc >> 16) & 0x7FFF parameter annotation objects
a tuple listing the parameter names for the annotations (only if there are only
annotation objects)
the code associated with the function (at TOS1)
the qualified name of the function (at TOS)

MAKE_CLOSURE(argc)
Creates a new function object, sets its __closure__ slot, and pushes it on the
stack. TOS is the qualified name of the function, TOS1 is the code associated
with the function, and TOS2 is the tuple containing cells for the closure’s
free variables. The function also has argc default parameters, which are found
below the cells.

BUILD_SLICE(argc)
Pushes a slice object on the stack. argc must be 2 or 3. If it is 2,
slice(TOS1, TOS) is pushed; if it is 3, slice(TOS2, TOS1, TOS) is pushed.
See the slice() built-in function for more information.

EXTENDED_ARG(ext)
Prefixes any opcode which has an argument too big to fit into the default two
bytes. ext holds two additional bytes which, taken together with the subsequent
opcode’s argument, comprise a four-byte argument, ext being the two
most-significant bytes.

CALL_FUNCTION_VAR(argc)
Calls a function. argc is interpreted as in CALL_FUNCTION. The top element on
the stack contains the variable argument list, followed by keyword and
positional arguments.

CALL_FUNCTION_KW(argc)
Calls a function. argc is interpreted as in CALL_FUNCTION. The top element on
the stack contains the keyword arguments dictionary, followed by explicit
keyword and positional arguments.

CALL_FUNCTION_VAR_KW(argc)
Calls a function. argc is interpreted as in CALL_FUNCTION. The top element on
the stack contains the keyword arguments dictionary, followed by the
variable-arguments tuple, followed by explicit keyword and positional arguments.

HAVE_ARGUMENT
This is not really an opcode. It identifies the dividing line between opcodes
which don’t take arguments < HAVE_ARGUMENT and those which do >= HAVE_ARGUMENT.

"""