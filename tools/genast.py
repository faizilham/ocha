from types import MethodType
output_dir = "src"

AST = {
    "Expr": [
        "Binary     -> left: Box<Expr>, operator: Token, right: Box<Expr>",
        "Grouping   -> expr: Box<Expr>",
        "Literal    -> value: Value",
        "Unary      -> operator: Token, expr: Box<Expr>",
        "Ternary    -> condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr>",
        "Variable   -> name: Token"
    ],
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

def define_ast(basename, type_data):
    with open(output_dir + "/" + basename.lower() + ".rs", "w") as writer:
        extend_writer(writer)

        writer.writeln("use token::{Token, Value};")
        writer.writeln()

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
"""
        writer.start_block("abstract class {0}".format(basename))

        # Visitor classes
        define_visitor(writer, basename, types)

        # AST Classes
        for _type in types:
            entries = _type.split(":")
            classname = entries[0].strip()
            fields = entries[1].strip() if len(entries) > 1 else ""
            define_type(writer, basename, classname, fields)

        # Base accept() method
        writer.writeln()
        writer.writeln("abstract <R> R accept(Visitor<R> visitor);")
        
        writer.end_block(True)"""

def define_visitor(writer, basename, types):
    writer.start_block("interface Visitor<R>");
    
    for _type in types:
        typename = _type.split(":")[0].strip()
        writer.writeln("R visit{0}{1}({0} {2});".format(typename, basename, basename.lower()))

    writer.end_block()

def define_type(writer, basename, classname, fieldlist):
    writer.writeln()
    writer.start_block("static class {0} extends {1}".format(classname, basename))

    # Constructor
    writer.start_block("{0}({1})".format(classname, fieldlist))

    fields = fieldlist.split(", ") if fieldlist else []
    for field in fields:
        name = field.split(" ")[1]
        writer.writeln("this.{0} = {0};".format(name))

    writer.end_block()

    # Visitor pattern
    writer.writeln()
    writer.start_block("<R> R accept(Visitor<R> visitor)")

    writer.writeln("return visitor.visit{0}{1}(this);".format(classname, basename));

    writer.end_block();

    # Fields
    writer.writeln()
    for field in fields:
        writer.writeln("final {0};".format(field))
    
    # end
    writer.end_block()

for (basename, types) in AST.items():
    define_ast(basename, types)
