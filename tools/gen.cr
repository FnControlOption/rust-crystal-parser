require "compiler/crystal/syntax"

include Crystal

# .as(Case).whens[0]
root = Parser.parse(<<-CRYSTAL)
case current_char
when '_'
  case next_char
  when '_'
    case next_char
    when 'D'
      if char_sequence?('I', 'R', '_', '_')
      end
    when 'E'
      if char_sequence?('N', 'D', '_', 'L', 'I', 'N', 'E', '_', '_')
      end
    when 'F'
      if char_sequence?('I', 'L', 'E', '_', '_')
      end
    when 'L'
      if char_sequence?('I', 'N', 'E', '_', '_')
      end
    else
      # scan_ident
    end
  else
  end

  scan_ident(start)
end
CRYSTAL

class Rustifier
  # @indent = 0
  # getter lines = [] of String

  def indent(s : String)
    s.lines.map { |l| "    #{l}" }.join('\n')
  end

  def terminate(s : String)
    if s.empty? || s.includes?('\n') || s.ends_with?(';')
      s
    else
      "#{s};"
    end
  end

  def transform(node : ASTNode)
    raise "unexpected #{node.class_desc} node:\n#{node}"
  end

  def transform(node : Nop)
    ""
  end

  def transform(node : Expressions)
    if node.keyword != Expressions::Keyword::None
      abort "expected no Expresssions keyword, got #{node.keyword}"
    end
    expressions = node.expressions.map { |exp| terminate transform(exp) }
    expressions.join("\n")
  end

  def transform(node : Return)
    if exp = node.exp
      # if (call = exp.as? Call) && call.name == "check_ident_or_keyword"
      #   <<-EOS
      #   #{terminate transform(exp)}
      #   return Ok(());
      #   EOS
      # else
      #   abort "unexpected return"
      # end
      "return #{transform(exp)};"
    else
      abort "expected return value"
    end
  end

  def transform(node : If)
    io = String::Builder.new
    io.puts "if #{transform(node.cond)} {"
    io.puts indent(transform(node.then))
    unless node.else.is_a? Nop
      io.puts "} else {"
      io.puts indent(transform(node.else))
    end
    io << '}'
    io.to_s
  end

  def transform(node : Case)
    abort "expected case condition" unless cond = node.cond
    io = String::Builder.new
    io.puts "match #{transform(cond)} {"
    node.whens.each do |when_|
      conds = when_.conds.map { |cond| transform(cond) }.join(" | ")
      io.puts indent <<-EOS
        #{conds} => {
        #{indent transform(when_.body)}
        }
        EOS
      # io.puts "    #{conds} => {"
      # io.puts "        #{transform(when_.body)}"
      # io.puts "    }"
    end
    if else_ = node.else
      io.puts indent <<-EOS
        _ => {
        #{indent transform(else_)}
        }
        EOS
    end
    io << "}"
    io.to_s
  end

  def transform(node : Call)
    if node.name == "=="
      abort "expected receiver for ==" unless obj = node.obj
      abort "expected 1 arg for ==" unless node.args.size == 1
      "#{transform(obj)} == #{transform(node.args[0])}"
    elsif obj = node.obj
      unless (ivar = obj.as? InstanceVar) && ivar.name == "@token"
        abort "unexpected call receiver: #{obj}"
      end

      case node.name
      when "type="
        unless node.args.size == 1
          abort "expected 1 arg for @token.type="
        end
        "self.token.kind = #{transform(node.args[0])}"
      else
        abort "unexpected call: @token.#{node.name}"
      end
    else
      case node.name
      when "current_char", "peek_next_char"
        unless node.args.size == 0
          abort "expected 0 args for #{node.name}"
        end
        "self.#{node.name}()"
      when "next_char"
        case node.args.size
        when 0
          "self.next_char()"
        when 1
          "self.next_char2(#{transform(node.args[0])})"
        else
          abort "expected 0..1 args for next_char"
        end
      when "check_ident_or_keyword"
        unless node.args.size == 2
          abort "expected 2 args for check_ident_or_keyword"
        end
        keyword, start = node.args
        unless keyword = keyword.as? SymbolLiteral
          abort "expected symbol for check_ident_or_keyword"
        end
        unless (start = start.as? Call) && start.name == "start"
          abort "expected start for check_ident_or_keyword"
        end
        keyword = keyword.value.camelcase
        "self.check_ident_or_keyword(Keyword::#{keyword}, start)"
      when "char_sequence?"
        args = node.args.map { |arg| transform(arg) }.join(", ")
        "self.char_sequence(&[#{args}])"
      when "scan_ident"
        unless node.args.size == 1
          abort "expected 1 arg for scan_ident"
        end
        start = node.args[0]
        unless (start = start.as? Call) && start.name == "start"
          abort "expected start for scan_ident"
        end
        "self.scan_ident(start)"
      else
        # "self.#{node.name}(...)"
        abort "unexpected call: #{node.name}"
        # <<-EOS
        # // TODO: implement #{node.name}
        # return self.unknown_token();
        # EOS
      end
    end
  end

  def transform(node : SymbolLiteral)
    case node.value
    when /^OP_/
      "Op(#{node.value[3..].downcase.camelcase})"
    when /^MAGIC_/
      "Magic(#{node.value[6..].downcase.camelcase})"
    else
      node.value.downcase.camelcase
    end
  end

  def transform(node : CharLiteral)
    node.value.inspect
  end

  # def puts(line)
  #   @lines << (" " * 4 * @indent + line)
  # end
end

r = Rustifier.new
puts r.transform(root)
# puts r.lines.join('\n')
# puts <<-EOS
# #{root.conds[0].as(CharLiteral).value.inspect} => {
#     match self.next_char() {
#         '' => {}
#     }
# }
# EOS

# # p .class_desc
# root.body.as(Case).whens.map do |w|
#   puts <<-EOS
#   #{w.conds[0].as(CharLiteral).value.inspect} => {}
#   EOS
# end
