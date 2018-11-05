from types import MethodType
output_dir = "src/ast"

AST = {
    "Expr": (
        [ "token::Token", "token::Literal"],
        [
            "Binary     -> left: Box<Expr>, operator: Token, right: Box<Expr>",
            "FuncCall   -> callee: Box<Expr>, args: Vec<Box<Expr>>",
            "Get        -> callee: Box<Expr>, operator: Token, member: Box<Expr>",
            "Grouping   -> expr: Box<Expr>",
            "Literal    -> value: Literal",
            "ListInit   -> exprs: Vec<Box<Expr>>",
            "Unary      -> operator: Token, expr: Box<Expr>",
            "Ternary    -> condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr>",
            "Variable   -> name: Token"
        ]
    ),

    "Stmt":(
        [ "ast::expr::Expr", "token::Token" ],
        [
            "Assignment -> name: Token, expr: Box<Expr>",
            "Block      -> body: Vec<Box<Stmt>>",
            "Break      -> ",
            "Expression -> expr: Box<Expr>",
            "FuncDecl   -> name: Token, args: Vec<Token>, body: Vec<Box<Stmt>>",
            "If         -> condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>>",
            "Print      -> exprs: Vec<Box<Expr>>",
            "Return     -> expr: Option<Box<Expr>>",
            "Set        -> get_expr: Box<Expr>, expr: Box<Expr>",
            "VarDecl    -> name: Token, expr: Box<Expr>",
            "While      -> condition: Box<Expr>, body: Box<Stmt>",
        ]
    )
}

META_IMPORT = []

META = [
    ("line", "i32")
]

TAB = " "*4

def extend_writer(writer):
    def inc_tab(self):
        self.tab += 1

    def dec_tab(self):
        self.tab -= 1

    def writeln(self, text = ""):
        tab = "" if text == "" else TAB*self.tab
        self.write("{0}{1}\n".format(tab, text))

    def start_block(self, text=""):
        self.writeln(text + " {")
        self.inc_tab()

    def end_block(self, eof=False):
        self.dec_tab()
        if not eof: self.writeln('}')
        else: self.write('}')

    writer.tab = 0
    writer.writeln = MethodType(writeln, writer)
    writer.inc_tab = MethodType(inc_tab, writer)
    writer.dec_tab = MethodType(dec_tab, writer)
    writer.start_block = MethodType(start_block, writer)
    writer.end_block = MethodType(end_block, writer)

def parse_types(type_data):
    types = []
    # Enum values
    for _type in type_data:
        entries = _type.split("->")
        classname = entries[0].strip()
        field_data = entries[1].strip().split(", ")
        fields = []

        for field in field_data:
            if not field:
                continue
            name, datatype = field.split(':')
            fields.append((name.strip(), datatype.strip()))

        types.append((classname, fields))

    return types

def define_ast(basename, imports, type_data):
    with open(output_dir + "/" + basename.lower() + ".rs", "w") as writer:
        extend_writer(writer)

        for im in META_IMPORT:
            writer.writeln("use {};".format(im))

        for im in imports:
            writer.writeln("use {};".format(im))

        if len(imports) > 0: writer.writeln()

        types = parse_types(type_data)

        # AST Struct
        writer.writeln("#[derive(Debug)]")
        writer.start_block("pub struct {}".format(basename))

        for (field, ftype) in META:
            writer.writeln("pub {}: {},".format(field, ftype))

        writer.writeln("pub node: {}Node,".format(basename))

        writer.end_block()

        # AST Node Enum
        writer.writeln("#[derive(Debug)]")
        writer.start_block("pub enum {}Node".format(basename))

        for (classname, fields) in types:
            items = ", ".join(['{}: {}'.format(*field) for field in fields])

            if items:
                writer.writeln("{} {{ {} }},".format(classname, items))
            else:
                writer.writeln("{},".format(classname))

        writer.end_block()
        writer.writeln()

        # Visitor Trait
        writer.start_block("pub trait {}Visitor<T>".format(basename))

        for (classname, fields) in types:
            items = ", ".join(['{}: &{}'.format(*field) for field in fields])
            params = "(&mut self, )"
            if items:
                writer.writeln("fn visit_{}(&mut self, {}) -> T;".format(classname.lower(), items))
            else:
                writer.writeln("fn visit_{}(&mut self) -> T;".format(classname.lower()))

        writer.end_block()
        writer.writeln()

        # Constructor
        writer.start_block("impl {}".format(basename))

        constructor_params = []
        constructor_fields = []
        for (field, ftype) in META:
            constructor_fields.append(field)
            constructor_params.append("{}: {}".format(field, ftype))

        constructor_params.append("node: {0}Node".format(basename))
        constructor_fields.append("node")

        writer.start_block("pub fn new({1}) -> {0}".format(basename, ", ".join(constructor_params)))


        writer.writeln("{}{{ {} }}".format(basename, ", ".join(constructor_fields)))
        writer.end_block()

        # Accept function
        writer.start_block("pub fn accept<T, Visitor: {0}Visitor<T>>({1}: &Box<{0}>, visitor: &mut Visitor) -> T".format(basename, basename.lower()))
        writer.start_block("match &{}.node".format(basename.lower()))

        for (classname, fields) in types:
            left_param = ", ".join(['{}'.format(fieldname) for (fieldname, _) in fields])
            right_param = ", ".join([fieldname for (fieldname, _) in fields])

            if left_param:
                pattern = "{0}Node::{1}{{{2}}}".format(basename, classname, left_param)
            else:
                pattern = "{0}Node::{1}".format(basename, classname)

            writer.writeln("{0} => visitor.visit_{1}({2}),".format(pattern, classname.lower(), right_param))
        writer.end_block()
        writer.end_block()
        writer.end_block()

for (basename, (imports, types)) in AST.items():
    define_ast(basename, imports, types)

with open(output_dir + "/mod.rs", "w") as writer:
    modules = sorted([basename.lower() for basename in AST])

    for module in modules:
        writer.write("pub mod {};\n".format(module))
