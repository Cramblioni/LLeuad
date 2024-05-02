# Spans

    Spans are how locations are represented in the interpreter. They cover both a
region of source file, the name of the source file, and a line/column marker.
This is done for error reporting.

## §1 Error reporting conventions

    Although spans represent both a region and point in code, their textual
rendering is context dependent. 

Location rendering
    The span is formatted as `file name:line:column`

Region Rendering
    The contents in the file enclosed by the region of the span is copied and
printed.

Section Rendering
    The lines of the source file that contain the span are rendered. The region
of the span gets underlined. In CLI output, then an extra line is added for
adding `~` as an underline.

## §2 Token Conventions

A span should enclose a token and point at the start of the first glyph.

## §3 Region and Point conventions

    The region of the span should be the minimal size to contain whatever the
textual representation of what the span is representing is. The various
rendering techniques above require this to operate properly.

    The point within the span should be the start of whatever is the most
distinguishing part of the span. This includes the spans of a node's children in
the AST.

### §3.1 AST node conventions

Expr::Ident, Expr::Int, Expr::Float, Expr::String, Expr::Bool
    Literal nodes always have the span pointing at their first character. The
region of the span should enclose the entire literal. In the case for
`Expr::String`, then the span points at the first quotation mark, even though
the literal is the text between.

Expr::Call
    the span should point at the opening parenthesis. The region of the span
should enclose the reciever and the argument list.

Expr::Attr, Expr::BinOp, Expr::UnOp
    The span should point at the operator (`.` in the case of `Expr::Attr`). The
region of the span encloses both the operator and the operand/s.

