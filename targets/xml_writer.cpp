#include <string>
#include "targets/xml_writer.h"
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated

//---------------------------------------------------------------------------

void til::xml_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::xml_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::xml_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  os() << std::string(lvl, ' ') << "<sequence_node size='" << node->size() << "'>" << std::endl;
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  process_literal(node, lvl);
}

void til::xml_writer::do_double_node(cdk::double_node * const node, int lvl) {
  process_literal(node, lvl);
}

void til::xml_writer::do_string_node(cdk::string_node * const node, int lvl) {
  process_literal(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_unary_operation(cdk::unary_operation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

void til::xml_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

void til::xml_writer::do_not_node(cdk::not_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

void til::xml_writer::do_allocation_node(til::allocation_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

void til::xml_writer::do_with_node(til::with_node * const node, int lvl) {
}

//---------------------------------------------------------------------------

void til::xml_writer::do_binary_operation(cdk::binary_operation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_add_node(cdk::add_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_div_node(cdk::div_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_le_node(cdk::le_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_and_node(cdk::and_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void til::xml_writer::do_or_node(cdk::or_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << ">" << node->name() << "</" << node->label() << ">" << std::endl;
}

void til::xml_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->lvalue()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("lvalue", lvl + 2);
  node->lvalue()->accept(this, lvl + 4);
  reset_new_symbol();
  closeTag("lvalue", lvl + 2);
  openTag("rvalue", lvl + 2);
  node->rvalue()->accept(this, lvl + 4);
  closeTag("rvalue", lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_pointer_index_node(til::pointer_index_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("base", lvl + 2);
  node->base()->accept(this, lvl + 4);
  closeTag("base", lvl + 2);
  openTag("index", lvl + 2);
  node->index()->accept(this, lvl + 4);
  closeTag("index", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_function_node(til::function_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << " type='" <<
  cdk::to_string(node->type()) << "' is_program='" << node->is_program() << "'>" << std::endl;
  openTag("arguments", lvl + 2);
  node->arguments()->accept(this, lvl + 4);
  closeTag("arguments", lvl + 2);
  openTag("declarations", lvl + 2);
  node->declarations()->accept(this, lvl + 4);
  closeTag("declarations", lvl + 2);
  openTag("instructions", lvl + 2);
  node->instructions()->accept(this, lvl + 4);
  closeTag("instructions", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_print_node(til::print_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;

  os() << std::string(lvl, ' ') << "<" << node->label() << " add_newline='" <<
  node->add_newline() << "'>" << std::endl;
  node->arguments()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_return_node(til::return_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  if (node->return_value() == nullptr) {
    os() << std::string(lvl, ' ') + "<" + node->label() + " />" << std::endl;
  } else {
    openTag(node, lvl);
    node->return_value()->accept(this, lvl + 2);
    closeTag(node, lvl);
  }
}

void til::xml_writer::do_stop_node(til::stop_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << " level='" <<
  node->level() << "' />" << std::endl;
}

void til::xml_writer::do_next_node(til::next_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << " level='" <<
  node->level() << "' />" << std::endl;
}

//---------------------------------------------------------------------------

void til::xml_writer::do_read_node(til::read_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') + "<" + node->label() + " />" << std::endl;
}

//---------------------------------------------------------------------------

void til::xml_writer::do_loop_node(til::loop_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("block", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("block", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_if_node(til::if_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  closeTag(node, lvl);
}

void til::xml_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->thenblock()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  openTag("else", lvl + 2);
  node->elseblock()->accept(this, lvl + 4);
  closeTag("else", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_declaration_node(til::declaration_node * const node, int lvl) {
  os() << std::string(lvl, ' ') << "<" << node->label() << " qualifier='" <<
  qualifier_name(node->qualifier()) << "' identifier='" << node->identifier() <<
  "' type='" << (node->type() != nullptr ? cdk::to_string(node->type()) : "inferred") <<
  "'>" << std::endl;
  
  if (node->initializer() == nullptr) {
    os() << std::string(lvl + 2, ' ') + "<initializer />" << std::endl;
  } else {
    openTag("initializer", lvl + 2);
    node->initializer()->accept(this, lvl + 4);
    closeTag("initializer", lvl + 2);
  }
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_block_node(til::block_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("declarations", lvl + 2);
  node->declarations()->accept(this, lvl + 4);
  closeTag("declarations", lvl + 2);
  openTag("instructions", lvl + 2);
  node->instructions()->accept(this, lvl + 4);
  closeTag("instructions", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_address_of_node(til::address_of_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_null_pointer_node(til::null_pointer_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') + "<" + node->label() + " />" << std::endl;
}

//---------------------------------------------------------------------------

void til::xml_writer::do_sizeof_node(til::sizeof_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void til::xml_writer::do_function_call_node(til::function_call_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("function", lvl + 2);
  if (node->function() == nullptr) {
    os() << std::string(lvl + 4, ' ') + "<recursive />" << std::endl;
  } else {
    node->function()->accept(this, lvl + 4);
  }
  closeTag("function", lvl + 2);
  openTag("arguments", lvl + 2);
  node->arguments()->accept(this, lvl + 4);
  closeTag("arguments", lvl + 2);
  closeTag(node, lvl);
}
