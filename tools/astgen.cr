require "compiler/crystal/syntax"

include Crystal

CRYSTAL_ROOT = ENV["CRYSTAL_ROOT"]? || "#{ENV["HOME"]}/Documents/crystal"

root = Parser.parse(File.read("#{CRYSTAL_ROOT}/src/compiler/crystal/syntax/ast.cr"))
root = root.as(Expressions).expressions[0].as(ModuleDef)
defs = root.body.as(Expressions).expressions
# pp defs.map(&.to_s.lines.first)
# puts defs[9].as(ClassDef).name
# puts "pub enum AstNodeEnum {"
found = false
defs.each do |node|
  next unless node = node.as? ClassDef
  next if node.abstract?
  name = node.name.to_s
  found ||= name == "ClassDef"
  next unless found
  name = "Self_" if name == "Self"
  # puts "    #{name},"
  if name.in? %w(NilableCast)
    puts <<-EOS
    Node!(
        #{name};
        // TODO
    );
    EOS
    puts
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
    next if call.name.in? %w(def_equals def_equals_and_hash def_hash)
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
                   when "Int32"       then "usize"
                   when "Location"    then "Location<'f>"
                   when "String"      then "Vec<char>"
                   when "Suffix"      then "GenericSuffix"
                   when "Token::Kind" then "TokenKind"
                   else
                     if field_type.in? %w(Arg AsmOperand Def NamedArgument Path Rescue Var)
                       "Box<#{field_type}<'f>>"
                     else
                       abort "unexpected type: #{field_type}"
                     end
                   end
      field_type = "Vec<#{field_type}>" if is_array
      field_type = "Option<#{field_type}>" if is_optional
      "pub #{field_name}: #{field_type},"
    when Assign
      assign = arg.as(Assign)
      field_name = assign.target.to_s
      default_value = assign.value.to_s
      "pub #{field_name}: ?,"
    else
      abort "expected TypeDeclaration or Assign for 'property', found #{arg.class_desc}"
    end
  end
  puts "Node!("
  puts "    #{name};"
  fields.each do |field|
    puts "    #{field}"
  end
  puts ");"
  puts
end
# puts "}"
