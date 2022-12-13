require "compiler/crystal/syntax"

include Crystal

# .as(Case).whens[0]
root = Parser.parse(<<-CRYSTAL)
case current_char
when '&'
  case next_char
  when '&'
    case next_char
    when '='
      next_char :OP_AMP_AMP_EQ
    else
      @token.type = :OP_AMP_AMP
    end
  when '='
    next_char :OP_AMP_EQ
  when '+'
    case next_char
    when '='
      next_char :OP_AMP_PLUS_EQ
    else
      @token.type = :OP_AMP_PLUS
    end
  when '-'
    case next_char
    when '='
      next_char :OP_AMP_MINUS_EQ
    else
      @token.type = :OP_AMP_MINUS
    end
  when '*'
    case next_char
    when '*'
      next_char :OP_AMP_STAR_STAR
    when '='
      next_char :OP_AMP_STAR_EQ
    else
      @token.type = :OP_AMP_STAR
    end
  else
    @token.type = :OP_AMP
  end
when '|'
  case next_char
  when '|'
    case next_char
    when '='
      next_char :OP_BAR_BAR_EQ
    else
      @token.type = :OP_BAR_BAR
    end
  when '='
    next_char :OP_BAR_EQ
  else
    @token.type = :OP_BAR
  end
when '^'
  case next_char
  when '='
    next_char :OP_CARET_EQ
  else
    @token.type = :OP_CARET
  end
end
CRYSTAL

class Rustifier
  # @indent = 0
  # getter lines = [] of String

  def indent(s : String)
    s.lines.map { |l| "    #{l}" }.join('\n')
  end

  def transform(node : ASTNode)
    abort "unexpected #{node.class_desc} node #{node}"
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
    io.puts "}"
    io.to_s
  end

  def transform(node : Call)
    if obj = node.obj
      unless (ivar = obj.as?(InstanceVar)) && ivar.name == "@token"
        abort "unexpected call receiver"
      end

      case node.name
      when "type="
        abort "expected 1 call arg" if node.args.size != 1
        "self.token.kind = #{transform(node.args[0])}"
      else
        abort "unexpected call: @token.#{node.name}"
      end
    else
      case node.name
      when "current_char"
        abort "expected 0 call args" if node.args.size > 0
        "self.current_char()"
      when "next_char"
        case node.args.size
        when 0
          "self.next_char()"
        when 1
          "self.next_char2(#{transform(node.args[0])})"
        else
          abort "expected 0..1 call args"
        end
      else
        # abort "unexpected call: #{node.name}"
        <<-EOS
        // TODO: implement #{node.name}
        return self.unknown_token();
        EOS
      end
    end
  end

  def transform(node : SymbolLiteral)
    case node.value
    when /^OP_/
      "Op(#{node.value[3..].downcase.camelcase})"
    else
      abort "unexpected symbol: #{node.value}"
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
