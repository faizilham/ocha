from types import MethodType
output_dir = "src/ast"

AST = {
    "Expr": (
        [ "std::rc::Rc", "token::Token", "value::Value"],
        [
            "Binary     -> left: Box<Expr>, operator: Token, right: Box<Expr>",
            "Grouping   -> expr: Box<Expr>",
            "Literal    -> value: Rc<Value>",
            "Unary      -> operator: Token, expr: Box<Expr>",
            "Ternary    -> condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr>",
            "Variable   -> name: Token"
        ]
    ),

    "Stmt":(
        [ "ast::expr::Expr" ],
        [
            "Expression -> expr: Box<Expr>",
            "If         -> condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>>",
            "Print      -> exprs: Vec<Box<Expr>>",
            "While      -> condition: Box<Expr>, body: Box<Stmt>",   
        ]
    )
}

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
            name, datatype = field.split(':')
            fields.append((name.strip(), datatype.strip()))

        types.append((classname, fields))
    
    return types

def define_ast(basename, imports, type_data):
    with open(output_dir + "/" + basename.lower() + ".rs", "w") as writer:
        extend_writer(writer)

        for im in imports:
            writer.writeln("use {};".format(im))

        if len(imports) > 0: writer.writeln()

        types = parse_types(type_data)
            
        # AST Enum
        writer.writeln("#[derive(Debug)]")
        writer.start_block("pub enum {}".format(basename))

        for (classname, fields) in types:
            items = ", ".join(['{}: {}'.format(*field) for field in fields])

            writer.writeln("{} {{ {} }},".format(classname, items))

        writer.end_block()
        writer.writeln()

        # Visitor Trait
        writer.start_block("pub trait {}Visitor<T>".format(basename))
        
        for (classname, fields) in types:
            items = ", ".join(['{}: &{}'.format(*field) for field in fields])
            params = "(&mut self, )"

            writer.writeln("fn visit_{}(&mut self, {}) -> T;".format(classname.lower(), items))
        
        writer.end_block()
        writer.writeln()

        # Accept function
        writer.start_block("impl {}".format(basename))
        writer.start_block("pub fn accept<T, Visitor: {0}Visitor<T>>({1}: &Box<{0}>, visitor: &mut Visitor) -> T".format(basename, basename.lower()))
        writer.start_block("match **{}".format(basename.lower()))

        for (classname, fields) in types:
            left_param = ", ".join(['ref {}'.format(fieldname) for (fieldname, _) in fields])
            right_param = ", ".join([fieldname for (fieldname, _) in fields])
            
            writer.writeln("{0}::{1}{{{2}}} => visitor.visit_{3}({4}),".format(basename, classname, left_param, classname.lower(), right_param))
        writer.end_block()
        writer.end_block()
        writer.end_block()

for (basename, (imports, types)) in AST.items():
    define_ast(basename, imports, types)

with open(output_dir + "/mod.rs", "w") as writer:
    for basename in AST:
        writer.write("pub mod {};\n".format(basename.lower()))