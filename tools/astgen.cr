require "compiler/crystal/syntax"

include Crystal

CRYSTAL_ROOT = ENV["CRYSTAL_ROOT"]? || "#{ENV["HOME"]}/Documents/crystal"

root = Parser.parse(File.read("#{CRYSTAL_ROOT}/src/compiler/crystal/syntax/ast.cr"))
root = root.as(Expressions).expressions[0].as(ModuleDef)
defs = root.body.as(Expressions).expressions
# pp defs.map(&.to_s.lines.first)
# puts defs[9].as(ClassDef).name
# puts "pub enum AstNodeEnum {"
start, stop = "TypeOf", "AsmOperand"
found = false
defs.each do |node|
  next unless node = node.as? ClassDef
  next if node.abstract?
  name = node.name.to_s
  found ||= start.nil? || name == start
  next unless found
  name = "Self_" if name == "Self"
  # puts "    #{name},"
  if superclass = node.superclass
    superclass = superclass.to_s
  else
    abort "expected superclass"
  end
  if superclass != "ASTNode"
    puts
    puts "#{superclass}!(#{name});"
    next
  end
  if name.in? %w(NilableCast)
    puts
    puts <<-EOS
    Node!(
        NilableCast;
        pub obj: AstNodeBox<'f>,
        pub to: AstNodeBox<'f>,
    );
    
    impl<'f> NilableCast<'f> {
        pub fn new(obj: AstNodeBox<'f>, to: AstNodeBox<'f>) -> Box<Self> {
            new_node! {
                obj: obj,
                to: to,
            }
        }
    }
    EOS
    next
  end
  case class_body = node.body
  when Expressions
    class_body = class_body.as(Expressions).expressions
  else
    class_body = [class_body]
  end
  fields = class_body.compact_map do |expression|
    next unless call = expression.as? Call
    next if call.name.in? %w(
      def_equals
      def_equals_and_hash
      def_hash
      record
      setter
    )
    unless call.name.in? %w(property property?)
      abort "expected 'property', found '#{call.name}'"
    end
    unless call.args.size == 1
      abort "expected only one argument for 'property'"
    end
    case arg = call.args.first
    when TypeDeclaration
      type_declaration = arg.as(TypeDeclaration)
      field_name = type_declaration.var.to_s
      is_optional = false
      case field_type = type_declaration.declared_type
      when Crystal::Union
        field_types = field_type.as(Crystal::Union).types
        # abort field_types.last.class_desc
        unless field_types.size == 2 && field_types.last.as(Crystal::Path).names == ["Nil"]
          abort "expected nilable type"
        end
        is_optional = true
        field_type = field_types.first.to_s
      else
        field_type = field_type.to_s
      end
      is_array = false
      if /^Array\((.*)\)$/ =~ field_type
        field_type = $1
        is_array = true
      end
      field_type = case field_type
                   when "ASTNode"     then "AstNodeBox<'f>"
                   when "Bool"        then "bool"
                   when "Char"        then "char"
                   when "Entry"       then "#{name}Entry<'f>"
                   when "Int32"       then "usize"
                   when "Location"    then "Location<'f>"
                   when "NumberKind"  then "NumberKind"
                   when "Regex::Options"
                    nil
                   when "String"      then "Vec<char>"
                   when "Suffix"      then "GenericSuffix"
                   when "Token::Kind" then "TokenKind"
                   when "Visibility"  then "Visibility"
                   when "When"
                    name == "Case" ? "When<'f>" : "#{name}When<'f>"
                   else
                     if field_type.in? %w(
                       Annotation
                       Arg
                       AsmOperand
                       Block
                       Call
                       Def
                       NamedArgument
                       Path
                       Rescue
                       Var
                     )
                       "Box<#{field_type}<'f>>"
                     else
                       abort "unexpected type: #{field_type}"
                     end
                   end
                   
      if field_type
        field_type = "Vec<#{field_type}>" if is_array
        field_type = "Option<#{field_type}>" if is_optional
      end
    when Assign
      assign = arg.as(Assign)
      field_name = assign.target.to_s
      default_value = assign.value.to_s
      field_type = case field_name
                   when "visibility" then "Visibility"
                   else
                     if field_name.in? %w(
                       assigns_special_var
                       calls_initialize
                       calls_previous_def
                       calls_super
                       expansion
                       has_parentheses
                       implicit
                       suffix
                       uses_block_arg
                     )
                       "bool"
                     else
                       abort "unknown field '#{field_name}'"
                     end
                   end
    else
      abort "expected TypeDeclaration or Assign for 'property', found #{arg.class_desc}"
    end
    field_name = "#{field_name}_" if field_name.in? %w(
      abstract
      const
      else
      struct
    )
    {field_name, field_type}
  end
  if fields.empty?
    puts
    puts "Node!(#{name});"
    puts
    puts <<-EOS
    impl<'f> #{name}<'f> {
        pub fn new() -> Box<Self> {
            new_node!()
        }
    }
    EOS
  else
    puts
    puts "Node!("
    puts "    #{name};"
    fields.each do |field_name, field_type|
      if field_type
        puts "    pub #{field_name}: #{field_type},"
      else
        puts "    // pub #{field_name}: ?,"
      end
    end
    puts ");"
    puts
    puts <<-EOS
    impl<'f> #{name}<'f> {
        pub fn new(
    EOS
    fields.each do |field_name, field_type|
      if field_type
        puts "        #{field_name}: #{field_type},"
      else
        puts "        // #{field_name}: ?,"
      end
    end
    puts "    ) -> Box<Self> {"
    puts "        new_node! {"
    fields.each do |field_name, field_type|
      if field_type
        puts "            #{field_name}: #{field_name},"
      else
        puts "            // #{field_name}: #{field_name},"
      end
    end
    puts <<-EOS
            }
        }
    }
    EOS
  end
  break if name == stop
end
# puts "}"
