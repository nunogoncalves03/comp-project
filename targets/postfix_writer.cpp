#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include "til_parser.tab.h"

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (inFunction()) {
    _pf.DOUBLE(node->value());    // stack
  } else {
    _pf.SDOUBLE(node->value());   // DATA segment
  }
}
void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void til::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));   // short circuit
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void til::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));    // short circuit
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (inFunction()) {
    _pf.INT(node->value());    // stack
  } else {
    _pf.SINT(node->value());   // DATA segment
  }
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (inFunction()) {
    /* leave the address on the stack */
    _pf.TEXT(_function_labels.top()); // return to the TEXT segment
    _pf.ADDR(mklbl(lbl1)); // the string to be stored
  } else {
    _pf.DATA(); // return to the DATA segment
    _pf.SADDR(mklbl(lbl1)); // the string to be stored
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    // void type has no size, but we'll allocate 1 byte
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    // void type has no size, but we'll allocate 1 byte
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}
void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    // void type has no size, but we'll allocate 1 byte
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    // void type has no size, but we'll allocate 1 byte
    _pf.INT(std::max(static_cast<size_t>(1), ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }

  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    // the difference between two pointers results in the number of objects of the type they reference
    // that fit between them, so we must divide by the size of the type
    auto lref = cdk::reference_type::cast(node->left()->type());
    // void type has no size, but we'll allocate 1 byte
    _pf.INT(std::max(static_cast<size_t>(1), lref->referenced()->size()));
    _pf.DIV();
  }
}

void til::postfix_writer::prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
}
void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void til::postfix_writer::prepareIDPredicateComparison(cdk::binary_operation_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}
void til::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  prepareIDPredicateComparison(node, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto symbol = _symtab.find(node->name()); // type checker ensured symbol exists

  if (symbol->qualifier() == tEXTERNAL) {
    _external_function_name = symbol->name();
  } else if (symbol->global()) {
    _pf.ADDR(node->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  node->lvalue()->accept(this, lvl);

  if (_external_function_name) {
    return; // name passed through this field; nothing in stack to be loaded
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, and strings
    _pf.LDINT();
  }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_node(til::function_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string function_label;
  if (node->is_program()) {
    function_label = "_main"; // RTS mandates that its name be "_main"
  } else {
    function_label = mklbl(++_lbl);
  }
  _function_labels.push(function_label);

  _pf.TEXT(_function_labels.top());
  _pf.ALIGN();
  if (node->is_program()) {
    _pf.GLOBAL("_main", _pf.FUNC());
  }
  _pf.LABEL(_function_labels.top());

  auto old_offset = _offset;
  _offset = 8; // function arguments start at +8
  _symtab.push(); // scope of args

  _in_function_args = true;
  node->arguments()->accept(this, lvl);
  _in_function_args = false;

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab);
  node->declarations()->accept(&fsc, lvl);
  node->instructions()->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  auto old_function_ret_label = _current_function_ret_label;
  _current_function_ret_label = mklbl(++_lbl);

  auto old_function_loop_labels = _current_function_loop_labels;
  _current_function_loop_labels = new std::vector<std::pair<std::string, std::string>>();

  _offset = 0; // local variables start at offset 0

  // FIXME: Not checking for final instructions (not a block_node)
  const auto block = new til::block_node(node->lineno(), node->declarations(), node->instructions());
  block->accept(this, lvl);

  // FIXME: should we do this? what about the non-zero returns?
  if (node->is_program()) {
    // return 0 if main has no return statement
    _pf.INT(0);
    _pf.STFVAL32();
  }

  _pf.ALIGN();
  _pf.LABEL(_current_function_ret_label);
  _pf.LEAVE();
  _pf.RET();

  delete _current_function_loop_labels;
  _current_function_loop_labels = old_function_loop_labels;
  _current_function_ret_label = old_function_ret_label;
  _offset = old_offset;
  _symtab.pop();
  _function_labels.pop();

  if (node->is_program()) {
    for (const std::string &name : _external_functions_to_declare) {
      _pf.EXTERN(name);
    }
  } else {
    // If the current function is a local variable of another function, we need
    // to place its address in the parent function's stack (because functions are expressions),
    // otherwise we need to place it in the DATA segment (because other than locally,
    // functions can only be declared in the global_decls section)
    if (inFunction()) {
      _pf.TEXT(_function_labels.top());
      _pf.ADDR(function_label);
    } else {
      _pf.DATA();
      _pf.SADDR(function_label);
    }
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->type()->size() > 0) {
    _pf.TRASH(node->argument()->type()->size());
  }
}

void til::postfix_writer::do_print_node(til::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));

    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl); // expression to print
    if (etype->name() == cdk::TYPE_INT) {
      _external_functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _external_functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (etype->name() == cdk::TYPE_STRING) {
      _external_functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    }
  }

  if (node->add_newline()) {
    _external_functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _external_functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    _external_functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int cond_label, end_label;

  _pf.ALIGN();
  _pf.LABEL(mklbl(cond_label = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(end_label = ++_lbl));

  _current_function_loop_labels->push_back(std::make_pair(mklbl(cond_label), mklbl(end_label)));
  node->block()->accept(this, lvl + 2);
  // if the loop block is a single instruction, we need to set this back to false
  // since this variable is only used while visiting a block_node, which will throw
  // an error if there was a final instruction before the last line, and set this to false otherwise
  _visited_final_instruction = false;
  _current_function_loop_labels->pop_back();

  _pf.JMP(mklbl(cond_label));
  _pf.ALIGN();
  _pf.LABEL(mklbl(end_label));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  // if the if block is a single instruction, we need to set this back to false
  // since this variable is only used while visiting a block_node, which will throw
  // an error if there was a final instruction before the last line, and set this to false otherwise
  _visited_final_instruction = false;
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl_else, lbl_end;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl_else = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _visited_final_instruction = false; // same as in if_node
  _pf.JMP(mklbl(lbl_end = ++_lbl));
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl_else));
  node->elseblock()->accept(this, lvl + 2);
  _visited_final_instruction = false; // same as in if_node
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl_end));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_declaration_node(til::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int offset = 0, typesize = node->type()->size(); // in bytes
  if (inFunction()) {
    _offset -= typesize;
    offset = _offset;
  } else if (_in_function_args) {
    offset = _offset;
    _offset += typesize;
  } else {
    // global variable
    offset = 0;
  }

  auto symbol = new_symbol();
  symbol->offset(offset);
  reset_new_symbol();

  if (inFunction()) {
    // function arguments and unitialized local variables don't need aditional treatment
    if (_in_function_args || node->initializer() == nullptr) {
      return;
    }

    node->initializer()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER)) {
      _pf.LOCAL(symbol->offset());
      _pf.STINT();
    } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (node->initializer()->is_typed(cdk::TYPE_INT))
        _pf.I2D();
      _pf.LOCAL(symbol->offset());
      _pf.STDOUBLE();
    } else {
      std::cerr << node->lineno() << ": should not happen: unknown type" << std::endl;
      return;
    }
  } else {
    if (symbol->qualifier() == tFORWARD || symbol->qualifier() == tEXTERNAL) {
      _external_functions_to_declare.insert(symbol->name());
      return;
    }
    
    _external_functions_to_declare.erase(symbol->name());

    if (node->initializer() == nullptr) {
      _pf.BSS();
      _pf.ALIGN();

      if (symbol->qualifier() == tPUBLIC) {
        _pf.GLOBAL(symbol->name(), _pf.OBJ());
      }

      _pf.LABEL(symbol->name());
      _pf.SALLOC(typesize);
    } else {
      if (
        dynamic_cast<cdk::integer_node*>(node->initializer()) == nullptr &&
        dynamic_cast<cdk::double_node*>(node->initializer()) == nullptr &&
        dynamic_cast<cdk::string_node*>(node->initializer()) == nullptr &&
        dynamic_cast<til::null_pointer_node*>(node->initializer()) == nullptr &&
        dynamic_cast<til::function_node*>(node->initializer()) == nullptr
      ) {
        std::cerr << node->lineno() << ": bad initializer for global variable '" +
          symbol->name() + "'" << std::endl;
        return;
      } else {
        _pf.DATA();
        _pf.ALIGN();

        if (symbol->qualifier() == tPUBLIC) {
          _pf.GLOBAL(symbol->name(), _pf.OBJ());
        }

        _pf.LABEL(symbol->name());

        if (node->is_typed(cdk::TYPE_DOUBLE) && node->initializer()->is_typed(cdk::TYPE_INT)) {
          // if the initializer is an integer, we need to convert it to double and
          // allocate space for a double
          auto int_node = dynamic_cast<cdk::integer_node*>(node->initializer());
          _pf.SDOUBLE(int_node->value());
        } else {
          node->initializer()->accept(this, lvl);
        }
      }
    }
  }
}

void til::postfix_writer::do_block_node(til::block_node * const node, int lvl) {
  _symtab.push(); // for block-local vars

  node->declarations()->accept(this, lvl + 2);

  _visited_final_instruction = false;
  for (size_t i = 0; i < node->instructions()->size(); i++) {
    if (_visited_final_instruction) {
      std::cerr << node->instructions()->node(i)->lineno() << ": unreachable code" << std::endl;
      return;
    }

    node->instructions()->node(i)->accept(this, lvl + 2);
  }
  _visited_final_instruction = false;

  _symtab.pop();
}

void til::postfix_writer::do_stop_node(til::stop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  size_t level = static_cast<size_t>(node->level());

  if (level == 0) {
    std::cerr << node->lineno() << ": invalid stop node argument" << std::endl;
    return;
  } else if (_current_function_loop_labels->size() < level) {
    std::cerr << node->lineno() << ": trying to break out of more loops than existing ones (" +
      std::to_string(_current_function_loop_labels->size()) + ")" << std::endl;
    return;
  }

  const size_t index = _current_function_loop_labels->size() - level;
  const std::string &label = std::get<1>(_current_function_loop_labels->at(index));
  _pf.JMP(label);

  _visited_final_instruction = true;
}

void til::postfix_writer::do_next_node(til::next_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  size_t level = static_cast<size_t>(node->level());

  if (level == 0) {
    std::cerr << node->lineno() << ": invalid next node argument" << std::endl;
    return;
  } else if (_current_function_loop_labels->size() < level) {
    std::cerr << node->lineno() << ": trying to skip an iteration of a non-existing loop (depth: " +
      std::to_string(_current_function_loop_labels->size()) + ")" << std::endl;
    return;
  }

  const size_t index = _current_function_loop_labels->size() - level;
  const std::string &label = std::get<0>(_current_function_loop_labels->at(index));
  _pf.JMP(label);

  _visited_final_instruction = true;
}

void til::postfix_writer::do_return_node(til::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // type_checker already made sure that the we are inside of a function and that
  // the return_node is compatible with the function's return type
  auto symbol = _symtab.find("@", 1);
  // til only supports single return values
  auto return_type = cdk::functional_type::cast(symbol->type())->output(0);

  if (return_type->name() != cdk::TYPE_VOID) {
    node->return_value()->accept(this, lvl + 2);

    if (return_type->name() == cdk::TYPE_INT || return_type->name() == cdk::TYPE_STRING
        || return_type->name() == cdk::TYPE_POINTER) {
      _pf.STFVAL32();
    } else if (return_type->name() == cdk::TYPE_DOUBLE) {
      if (node->return_value()->type()->name() == cdk::TYPE_INT) _pf.I2D();
      _pf.STFVAL64();
    } else {
      std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
      return;
    }
  }

  _pf.JMP(_current_function_ret_label);
  _visited_final_instruction = true;
}

void til::postfix_writer::do_null_pointer_node(til::null_pointer_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (inFunction()) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

void til::postfix_writer::do_address_of_node(til::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // since the argument is an lvalue, it is already an address
  node->argument()->accept(this, lvl + 2);
}

void til::postfix_writer::do_sizeof_node(til::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _pf.INT(node->argument()->type()->size());
}

void til::postfix_writer::do_pointer_index_node(til::pointer_index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

void til::postfix_writer::do_function_call_node(til::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::shared_ptr<cdk::functional_type> func_type;
  if (node->function() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    func_type = cdk::functional_type::cast(symbol->type());
  } else {
    func_type = cdk::functional_type::cast(node->function()->type());
  }

  size_t args_size = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (func_type->input(ax)->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      args_size += func_type->input(ax)->size();
    }
  }

  _external_function_name = std::nullopt;
  if (node->function() == nullptr) { // recursive call; "@"
    _pf.ADDR(_function_labels.top());
  } else {
    node->function()->accept(this, lvl);
  }

  if (_external_function_name) {
    _pf.CALL(*_external_function_name);
    _external_function_name = std::nullopt;
  } else {
    _pf.BRANCH();
  }

  if (args_size != 0) {
    _pf.TRASH(args_size);
  }

  if (func_type->output(0)->name() == cdk::TYPE_DOUBLE) {
    _pf.LDFVAL64();
  } else if (func_type->output(0)->name() != cdk::TYPE_VOID) {
    _pf.LDFVAL32();
  }
}

void til::postfix_writer::do_allocation_node(til::allocation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
  auto ref = cdk::reference_type::cast(node->type())->referenced();
  // void type has no size, but we'll allocate 1 byte
  _pf.INT(std::max(static_cast<size_t>(1), ref->size()));
  _pf.MUL();
  _pf.ALLOC(); // allocate
  _pf.SP();    // put base pointer in stack (reference to the allocated memory)
}
