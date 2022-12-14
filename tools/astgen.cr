require "compiler/crystal/syntax"

include Crystal

CRYSTAL_ROOT = ENV["CRYSTAL_ROOT"]? || "#{ENV["HOME"]}/Documents/crystal"

root = Parser.parse(File.read("#{CRYSTAL_ROOT}/src/compiler/crystal/syntax/ast.cr"))
root = root.as(Expressions).expressions[0].as(ModuleDef)
defs = root.body.as(Expressions).expressions
# pp defs.map(&.to_s.lines.first)
puts defs[9].as(ClassDef).name
