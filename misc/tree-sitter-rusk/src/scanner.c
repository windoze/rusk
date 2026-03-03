#include <tree_sitter/parser.h>
#include <wctype.h>

enum TokenType {
  BLOCK_COMMENT,
};

void *tree_sitter_rusk_external_scanner_create(void) { return NULL; }
void tree_sitter_rusk_external_scanner_destroy(void *p) { (void)p; }
void tree_sitter_rusk_external_scanner_reset(void *p) { (void)p; }
unsigned tree_sitter_rusk_external_scanner_serialize(void *p, char *buffer) {
  (void)p;
  (void)buffer;
  return 0;
}
void tree_sitter_rusk_external_scanner_deserialize(void *p, const char *buffer, unsigned length) {
  (void)p;
  (void)buffer;
  (void)length;
}

static bool scan_block_comment(TSLexer *lexer) {
  if (lexer->lookahead != '/') return false;
  lexer->advance(lexer, false);
  if (lexer->lookahead != '*') return false;
  lexer->advance(lexer, false);

  unsigned depth = 1;
  while (depth > 0) {
    if (lexer->eof(lexer)) return false;

    if (lexer->lookahead == '/') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '*') {
        lexer->advance(lexer, false);
        depth++;
        continue;
      }
      continue;
    }

    if (lexer->lookahead == '*') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '/') {
        lexer->advance(lexer, false);
        depth--;
        continue;
      }
      continue;
    }

    lexer->advance(lexer, false);
  }

  lexer->result_symbol = BLOCK_COMMENT;
  lexer->mark_end(lexer);
  return true;
}

bool tree_sitter_rusk_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  (void)payload;

  if (valid_symbols[BLOCK_COMMENT]) {
    return scan_block_comment(lexer);
  }

  return false;
}
